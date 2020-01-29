import json

output_list = []
with open('moves.json') as json_file:
    moves = json.load(json_file)
    # for k, v in data.items():
    #     # print(p)
    #     print(v)
    #     # output_list.append(v)
    print(len(moves))
    count = 0

    flags_set = set()
    for move in moves:
        # if "secondary" in move:
        # if "num" in move:
        if "flags" in move:
            for k in move["flags"]:
                flags_set.add(k)
        count += 1
    print(count)
    print(list(flags_set).sort())
    print(sorted(list(flags_set)))
# with open('moves_list.json', 'w') as outfile:
#     json.dump(output_list, outfile)
