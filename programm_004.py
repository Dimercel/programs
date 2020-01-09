#!/usr/bin/env python3

"""
Реализация "Бинарного кода Грея без циклов" из книги Д.Э. Кнута "Искусство программирования. Том 4А.
Комбинаторные алгоритмы", стр. 338.
"""


def gray_code(code, focus):
    print("".join(map(str, list(reversed(code)))))

    j = focus[0]
    focus[0] = 0

    if j == len(code):
        return
    else:
        focus[j] = focus[j + 1]
        focus[j + 1] = j + 1

    code[j] = 1 - code[j]
    gray_code(code, focus)

size = int(input('Укажите размер кортежа: '))
gray_code([0] * size, [x for x in range(size + 1)])
