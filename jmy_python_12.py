# I can't work out how to parse an unknown JSON in Haskell so I will have to do
# it another language
import json

def parseSum(dt, mode=0, part2=False):
  total = 0
  for x in dt:
    if mode == 0:
      x = dt[x]

    if (mode == 0 and part2 and x == "red"):
        return 0

    if type(x) is list:
      total += parseSum(x, 1, part2)  
    elif type(x) is int:
      total += x
    elif type(x) is dict:
      total += parseSum(x, 0, part2)

  return total

with open('input_12.txt') as json_file:
    data = json.load(json_file)

    print("Part 1: " + str(parseSum(data)))
    print("Part 2: " + str(parseSum(data,0,True)))
