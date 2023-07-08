"""

This bot uses all options provided by the Poe protocol. 

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

MODEL_URL = "mcai/dreamshaper-v6-img2img:c7959eb3a86c09b449dacc11ce8bba295fda466fc6935ab8709e35f4f48c980c"

_WAIT_TIMEOUT_S = 1


def parse_text(txt):
    # Define a regular expression to match the fields and their values
    pattern = re.compile(r'(image|prompt):\s*"([^"]+)"')
    matches = pattern.findall(txt)
    result = {}

    for field, value in matches:
        result[field] = value
    return result

core_positive_prompt = """
portrait closeup, best quality, intricately detailed, 
moe manga style, finely detailed features perfect art,
professional majestic impressionism oil painting by Waterhouse, 
John Constable, Ed Blinkey, Atey Ghailan, Studio Ghibli, 
by Jeremy Mann, Greg Manchess, Antonio Moro, trending on ArtStation, 
trending on CGSociety, cinematic lighting, hand drawn, hand colored.
"""

alternative_prompt = """
portrait closeup, best quality,
moe manga style, finely detailed features perfect art,
anime style, 8k, artwork in the style of guweiz, 
cinematic lighting, hand drawn, hand colored.
"""

negative_prompt = """
disfigured, kitsch, ugly, oversaturated, greain, low-res, deformed, blurry, bad anatomy, 
poorly drawn face, mutation, mutated, extra limb, missing limb, 
floating limbs, disconnected limbs, malformed hands, extra fingers, poorly drawn hands,
"""

def error_message():
    msg = textwrap.dedent(f"""
    Sorry, I cannot parse your input. Please try again and make sure your input has the format:

    ```python
    image: "<image_public_url>"
    prompt: (Optional) "<your prompt here>" #no worry, we will generate an anime of you to start
    ```

    """
    )
    return msg

def _get_complete_message(second, input_url, output_url):
    _COMPLETE_MESSAGE = f"""
    Completed! (took {second}s)

    This is you:

    ![]({input_url})

    This is the anime version of yourself.

    ![]({output_url})

    """
    return textwrap.dedent(_COMPLETE_MESSAGE)

class AnimeYourself(PoeBot):
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
        else:
            ### call the model to get results:
            input_prompt = "" if 'prompt' not in input_dict else input_dict['prompt']

            generated_image_task = asyncio.create_task(
                self._generate_image(
                    image_url = input_dict['image'], 
                    prompt = "mksks style," + input_prompt + "," + alternative_prompt
                )
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

            if len(output) != 1:
                yield self.replace_response_event(
                    textwrap.dedent(
                        f"""

                        Sorry, something seems to go wrong.

                        Please don't blame the developer. He's trying ᕕ( ᐛ )ᕗ.

                        But he does want you to know that you look amazing who you are.

                        ![]({input_dict['image']})
                        """
                    )
                )
            else:
                yield self.replace_response_event(
                    textwrap.dedent(
                        _get_complete_message(
                            second = i, 
                            input_url = input_dict['image'],
                            output_url = output[0])
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
                    "negative_prompt": negative_prompt,
                    "num_inference_steps": 50,
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
    run(AnimeYourself())