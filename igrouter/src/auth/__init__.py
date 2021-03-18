from instaloader import (
    Instaloader,
    TwoFactorAuthRequiredException,
    ConnectionException,
    Profile,
    BadCredentialsException,
)
from oathtool import generate_otp
from auth.credential import CredentialsLoader, InvalidCredentialsException
from lib.either import Left, Right, Either
from typing import Dict, Union
from os.path import expanduser

credentials_file_path = expanduser("~") + "/.igmng/credentials.toml"


def login(
    is_interact_authenicationable: bool = False,
) -> Union[Left[str], Right[Profile]]:
    cl = CredentialsLoader(credentials_file_path)
    credentials: Dict[str, str]
    try:
        credentials = cl.load()
    except InvalidCredentialsException as e:
        return Left(str(e))

    L = Instaloader()

    try:
        L.login(credentials["user_id"], credentials["password"])
    except TwoFactorAuthRequiredException:
        code: str
        if credentials["oath_key"] is None:
            if not is_interact_authenicationable:
                return Left(
                    "asked for two-step verification, but there is no OATH-token listed"
                )
            else:
                code = input("Required two-factor authenication: ")
        else:
            code = generate_otp(credentials["oath_key"])
        try:
            L.two_factor_login(code)
        except Exception as e:
            return Left(str(e))
    except ConnectionException as e:
        return Left(str(e))
    except BadCredentialsException as e:
        return Left(str(e))

    return Right(Profile.from_username(L.context, credentials["user_id"]))
