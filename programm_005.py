#!/usr/bin/env python3

"""Реализация двоичного поиска с поэтапным выводом"""

import random


def binary_search(list, elem):
    border = int(len(list) / 2)

    if list[border] == elem:
        print(f"Item found: {elem}")
        return elem

    if elem < list[border]:
        print(f"Element between {list[0]} and {list[border-1]}")
        return binary_search(list[:border], elem)

    print(f"Element between {list[border]} and {list[-1]}")
    return binary_search(list[border:], elem)


data = sorted([random.randint(0, 100) for x in range(50)])
required = data[random.randint(0, 49)]

print(f"Data: {data}")
print(f"Required element is {required}")

binary_search(data, required)
