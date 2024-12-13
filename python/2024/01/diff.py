import sys
from typing import Iterable


def lines() -> Iterable[list[str]]:
    for line in sys.stdin:
        yield line.split()


def diff(pair: tuple[int, int]) -> int:
    return abs(pair[0] - pair[1])


pairs: list[tuple[int, int]] = [(int(a), int(b)) for a, b in lines()]


left, right = (sorted(list_) for list_ in zip(*pairs))
print(sum(map(diff, zip(left, right))))
