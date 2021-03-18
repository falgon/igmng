import toml


class InvalidCredentialsException(Exception):
    def __init__(self, fpath):
        self.fpath = fpath

    def __str__(self):
        return f"{self.fpath} is an invalid credentials file"


class CredentialsLoader:
    def __init__(self, fpath):
        self.fpath = fpath

    def load(self):
        tomlobj = toml.load(self.fpath)
        if tomlobj["credentials"] is None:
            raise InvalidCredentialsException(self.fpath)
        credentials = tomlobj["credentials"]
        if credentials["user_id"] is None or credentials["password"] is None:
            raise InvalidCredentialsException(self.fpath)
        return credentials
