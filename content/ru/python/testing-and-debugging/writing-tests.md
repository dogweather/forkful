---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:07.060692-07:00
description: "\u041A\u0430\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C \u0432\u0441\u0442\u0440\u043E\
  \u0435\u043D\u043D\u044B\u0439 \u0432 Python \u0444\u0440\u0435\u0439\u043C\u0432\
  \u043E\u0440\u043A `unittest`."
lastmod: '2024-03-13T22:44:44.276906-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u0435\u043C \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\
  \u0439 \u0432 Python \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A `unittest`."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

## Как:
Давайте используем встроенный в Python фреймворк `unittest`.

```Python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add_integers(self):
        self.assertEqual(add(1, 2), 3)

    def test_add_strings(self):
        self.assertEqual(add('abc', 'def'), 'abcdef')

if __name__ == '__main__':
    unittest.main()
```

Запустите его, и вы увидите что-то вроде:

```
..
----------------------------------------------------------------------
Пройдено 2 теста за 0.001с

OK
```

Две точки означают, что два теста пройдены. Все хорошо.

## Погружение
Тестирование на Python стало популярным с `unittest` (вдохновленным JUnit от Java). Сейчас есть `pytest` и `nose`, более современные инструменты с более простым синтаксисом и лучшими функциями. При написании тестов помните: изолируйте тестовые случаи, тестируйте крайние случаи и мокируйте внешние зависимости, чтобы сосредоточиться на логике вашего кода, а не на внешнем мире.

## Смотрите также
Углубитесь в тестирование с помощью следующих ресурсов:

- Документация по `unittest` от Python: https://docs.python.org/3/library/unittest.html
- `pytest` для более современного подхода: https://docs.pytest.org/en/latest/
- Мокирование в тестах с помощью `unittest.mock`: https://docs.python.org/3/library/unittest.mock.html
