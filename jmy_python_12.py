# I can't work out how to parse an unknown JSON in Haskell so I w
import json

def arrSum(arr, part2):
  total = 0
  for x in arr:
    if type(x) is str:
      total += 0
    if type(x) is list:
      total += arrSum(x,part2)  
    if type(x) is int:
      total += x
    if type(x) is dict:
      total += dictsum(x,part2)  

  return total 

def dictsum(dt, part2=False):
  total = 0
  for x in dt:
    k = dt[x]
    if (part2):
      if k == "red":
        return 0

    if type(k) is str:
      total += 0
    if type(k) is list:
      total += arrSum(k,part2)  
    if type(k) is int:
      total += k 
    if type(k) is dict:
      total += dictsum(k,part2)

  return total

with open('input_12.txt') as json_file:
    data = json.load(json_file)

    print("Part 1: ")
    print(dictsum(data))
    print("Part 2: ")
    print(dictsum(data,True))
