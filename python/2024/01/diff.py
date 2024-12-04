import sys
from typing import Iterable


def lines() -> Iterable[str]:
    for line in sys.stdin:
        yield line.split()


def diff(pair: tuple[int, int]) -> int:
    return abs(pair[0] - pair[1])


pairs: list[tuple[int, int]] = [
    (int(a), int(b))
    for a, b in lines()
]


left, right = (sorted(l) for l in zip(*pairs))
print(sum(map(diff, zip(left, right))))
