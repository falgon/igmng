from typing import TypeVar, Callable, Generic, Union, NewType

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")

Either = NewType("Either", Union["Left[A]", "Right[C]"])


class Left(Generic[A]):
    def __init__(self, val: A):
        self.__val = val

    @property
    def from_right(self) -> A:
        raise Exception("left unwrapped")

    @property
    def from_left(self) -> A:
        return self.__val

    def flat_map(self, f: Callable[[B], Either]) -> Either:
        return self

    def is_left(self) -> bool:
        return True

    def is_right(self) -> bool:
        return False

    def __str__(self) -> str:
        return f"Left({self.__val})"


class Right(Generic[B]):
    def __init__(self, val: B):
        self.__val = val

    @property
    def from_right(self) -> B:
        return self.__val

    @property
    def from_left(self) -> B:
        raise Exception("right unwrapped")

    def flat_map(self, f: Callable[[B], Either]) -> Either:
        return f(self.__val)

    def is_left(self) -> bool:
        return False

    def is_right(self) -> bool:
        return True

    def __str__(self) -> str:
        return f"Right({self.__val})"
