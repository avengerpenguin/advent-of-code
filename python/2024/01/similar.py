import sys
from collections import Counter
from typing import Iterable


def lines() -> Iterable[str]:
    for line in sys.stdin:
        yield line.split()


pairs: list[tuple[int, int]] = [
    (int(a), int(b))
    for a, b in lines()
]


left, right = zip(*pairs)
counts = Counter(right)


print(sum([
    x * counts.get(x, 0)
    for x in left
]))
