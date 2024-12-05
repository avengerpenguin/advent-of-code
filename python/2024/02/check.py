import sys
from typing import Iterable


def lines() -> Iterable[str]:
    for line in sys.stdin:
        yield [int(x) for x in line.split()]


def check_safe(line):
    first, second, *_ = line
    if first == second:
        return False
    else:
        increasing = second > first

    for first, second in zip(line, line[1:]):
        if increasing:
            if second <= first:
                return False
            if second - first > 3:
                return False

        else:
            if first <= second:
                return False
            if first - second > 3:
                return False

    return True


def check_safe_with_removal(line):
    if check_safe(line):
        return True

    for n in range(len(line)):
        new_list = line[:n] + line[n+1:]
        if check_safe(new_list):
            return True

    return False


lines = list(lines())

print(sum(check_safe(line) for line in lines))
print(sum(check_safe_with_removal(line) for line in lines))
