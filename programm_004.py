#!/usr/bin/env python3

"""
Реализация "Бинарного кода Грея без циклов" из книги Д.Э. Кнута "Искусство программирования. Том 4А.
Комбинаторные алгоритмы", стр. 338.
"""

class GrayCode:

    def __init__(self, size):
        self.__size = size

    def __iter__(self):
        j = 0
        code = [0] * self.__size
        focus = [x for x in range(self.__size + 1)]
        while j != self.__size:
            yield tuple(reversed(code))

            j = focus[0]
            focus[0] = 0

            if j == self.__size:
                break
            else:
                focus[j] = focus[j + 1]
                focus[j + 1] = j + 1

            code[j] = 1 - code[j]


size = int(input('Укажите размер кортежа: '))

for code in GrayCode(size):
    print("".join(map(str, code)))
