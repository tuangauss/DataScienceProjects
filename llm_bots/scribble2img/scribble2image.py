"""

This bot uses all options provided by the Poe protocol. You can use it to get examples
of all the protocol has to offer.

"""
from __future__ import annotations

import asyncio
import json
from typing import AsyncIterable

from fastapi_poe import PoeBot, run
from fastapi_poe.types import (
    ContentType,
    QueryRequest,
    ReportFeedbackRequest,
    SettingsRequest,
    SettingsResponse,
)
import os
import replicate
import re
import random
import textwrap
import asyncio
from sse_starlette.sse import ServerSentEvent

SETTINGS = SettingsResponse(
    context_clear_window_secs=60 * 60, allow_user_context_clear=True
)

MODEL_URL = "jagilley/controlnet-scribble:435061a1b5a4c1e26740464bf786efdfa9cb3a3ac488595a2de23e143fdb0117"

_WAIT_TIMEOUT_S = 1

encouraging_msgs = [
    "The developer loves your scribble 😍",
    "With the drawing skill like this, do you even need this bot?",
    "Wow! Your scribbling skill is hella strong",
    "Interesting interesting! I gotcha",
    "Your doodle is simply world class. Let me see what else I can add."
]

def _get_complete_message(second, encouraging_msg, input_url, output_url):
    _COMPLETE_MESSAGE = f"""
    Completed! (took {second}s)

    This is the original. {encouraging_msg}:

    ![]({input_url})

    This is your scribble brought to life:

    ![]({output_url})

    """
    return _COMPLETE_MESSAGE


def parse_text(txt):
    # Define a regular expression to match the fields and their values
    pattern = re.compile(r'(image|prompt):\s*"([^"]+)"')
    matches = pattern.findall(txt)
    result = {}

    for field, value in matches:
        result[field] = value
    return result

def error_message(missing_image=True, image_url=None):
    missing_piece = "image" if missing_image else "prompt"
    if image_url:
        additional_txt = f"""

    But I just wanna say that I love your scribble.

    ![]({image_url})
        """
    else:
        additional_txt = ""
    msg = textwrap.dedent(f"""
    Sorry, I cannot parse your {missing_piece}. Please try again and make sure your input has the format:

    ```python
    image: "<image_public_url>"
    prompt: "<your prompt here>"
    ```

    {additional_txt}
    """
    )
    return msg

class Scribble2ImageBot(PoeBot):

    async def get_response(self, query: QueryRequest) -> AsyncIterable[ServerSentEvent]:
        """Return an async iterator of events to send to the user."""
        last_message = query.query[-1].content.lower()
        response_content_type: ContentType = ("text/markdown")
        yield self.meta_event(
            content_type=response_content_type,
            linkify=False,
            refetch_settings=False,
            suggested_replies=False,
        )

        input_dict = parse_text(last_message)
        if "image" not in input_dict:
            yield self.text_event(error_message())
        elif "prompt" not in input_dict:
            yield self.text_event(error_message(
                missing_image=False,
                image_url = input_dict['image']
                )
            )
        else:
            ### call the model to get results:
            generated_image_task = asyncio.create_task(
                self._generate_image(input_dict['image'], input_dict['prompt'])
            )

            i = 0
            while True:
                done, _ = await asyncio.wait(
                    [generated_image_task], timeout=_WAIT_TIMEOUT_S
                )
                if done:
                    output = done.pop().result()
                    break
                yield self.replace_response_event(f"Generating your image: {i}s elapsed...")
                i += 1

            if len(output) != 2:
                yield self.replace_response_event(
                    textwrap.dedent(
                        f"""

                        Sorry, something seems to go wrong.

                        Please don't blame the developer. He's trying ᕕ( ᐛ )ᕗ.

                        But he does want you to know that he loves your scribble.

                        ![]({input_dict['image']})
                        """
                    )
                )
            else:
                yield self.replace_response_event(
                    textwrap.dedent(
                        _get_complete_message(
                            second = i, 
                            encouraging_msg = random.choice(encouraging_msgs), 
                            input_url = input_dict['image'], 
                            output_url = output[1])
                    )
                )

    async def _generate_image(self, image_url: str, prompt: str):
        loop = asyncio.get_running_loop()
        output = await loop.run_in_executor(
            None,
            lambda: replicate.run(
                        MODEL_URL,
                        input={
                            "image": image_url, 
                            "prompt": prompt,
                        }
                    )
        )
        return output


    async def on_feedback(self, feedback: ReportFeedbackRequest) -> None:
        """Called when we receive user feedback such as likes."""
        print(
            f"User {feedback.user_id} gave feedback on {feedback.conversation_id}"
            f"message {feedback.message_id}: {feedback.feedback_type}"
        )

    async def get_settings(self, settings: SettingsRequest) -> SettingsResponse:
        """Return the settings for this bot."""
        return SETTINGS


if __name__ == "__main__":
    run(Scribble2ImageBot())