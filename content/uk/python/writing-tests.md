---
title:                "Написання тестів"
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Тестування коду - це перевірка, чи правильно працює програма. Програмісти пишуть тести, щоб виявляти помилки, забезпечити якість та спростити подальші зміни у коді.

## Як це робити:
```Python
import unittest

def add(x, y):
    return x + y

class SimpleTest(unittest.TestCase):
    def test_add_function(self):
        self.assertEqual(add(3, 4), 7)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(2, 2), 5)

if __name__ == '__main__':
    unittest.main()
```
Вивід:
```
...
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```
## Поглиблене вивчення:
Тестування коду присутнє з часів початку програмування. Альтернативи `unittest` в Python включають `pytest` і `nose2`. Ідея полягає в автоматизації перевірок та ізоляції функцій для точного виявлення помилок. Хороший тест покриває типові випадки, крайові умови та помилкові вхідні дані.

## Додатково:
- Документація `unittest`: https://docs.python.org/3/library/unittest.html
- Документація `pytest`: https://docs.pytest.org/en/latest/
- Як писати ефективні тести: https://realpython.com/python-testing/