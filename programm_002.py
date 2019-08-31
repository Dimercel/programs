"""
Реализация "лоскутного одеяла" из книги Д.Э. Кнута "Искусство программирования. Том 4А. Комбинаторные
алгоритмы", стр. 167. В этом примере иллюстрируется какие замысловатые изображения можно получить
используя лишь логические и арифметические операции. Сам алгоритм разработан D. Sleator, 1976.
"""

from PIL import Image

size = 1080
img = Image.new('RGB', (size, size))

pixels = []
for x in range(size):
    for y in range(size):
        val = x ^ (~y)
        val = val & ((x - size) >> 3)
        val *= val

        color_val = (val >> 12) & 1
        if color_val == 0:
            pixels.append(0xffffff)
        else:
            pixels.append(0x000000)

img.putdata(pixels)
img.save('programm_002_image.png')
