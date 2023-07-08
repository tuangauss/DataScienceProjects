from fastapi_poe import make_app
import modal
from modal import Image, Stub, asgi_app
from anime_yourself import AnimeYourself
import os

# specific to hosting with modal.com
image = Image.debian_slim().pip_install_from_requirements(
    "requirements.txt"
)
stub = Stub("animeyourself")


@stub.function(image=image, secret=modal.Secret.from_name("my-replicate-key"))
@asgi_app()
def fastapi_app():
    bot = AnimeYourself()
    app = make_app(bot, allow_without_key=True)
    return app

