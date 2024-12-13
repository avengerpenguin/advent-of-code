import sys
import re


print(
    sum(
        int(x) * int(y)
        for x, y in re.findall(
            r"mul\((\d+),(\d+)\)",
            re.sub(r"don't\(\).*?($|do\(\))", "", sys.stdin.read(), flags=re.DOTALL),
        )
    )
)
