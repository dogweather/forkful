---
title:                "Написание тестов"
aliases: - /ru/python/writing-tests.md
date:                  2024-01-29T00:06:07.060692-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Написание тестов означает создание кода для проверки корректности работы другого кода. Мы делаем это для выявления ошибок, обеспечения надежности и уменьшения страха перед обновлениями.

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
