import sys
import re

print(sum(
    int(x) * int(y)
    for x, y in
    re.findall(r"mul\(([0-9]+),([0-9]+)\)", sys.stdin.read())
))
