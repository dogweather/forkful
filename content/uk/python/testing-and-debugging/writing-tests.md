---
title:                "Письмо тестів"
date:                  2024-02-03T19:31:57.586633-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Написання тестів на Python включає створення автоматизованих скриптів для перевірки правильності вашого коду. Програмісти роблять це, щоб переконатися, що їхні функції або класи працюють як очікується в різних умовах, що допомагає виявляти помилки на ранньому етапі та полегшує подальше обслуговування та рефакторинг.

## Як:
Python має вбудований модуль для написання тестів, який називається `unittest`. Ось як ви можете його використовувати для тестування простої функції:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Має бути 12")

if __name__ == '__main__':
    unittest.main()
```

Коли ви запустите цей тестовий скрипт, ви повинні побачити вивід, що показує, чи пройшли ваші тести (або ні).

Для більш сучасних та виразних тестів ви можете використовувати сторонню бібліотеку, як-от `pytest`. Спочатку вам потрібно буде встановити її за допомогою pip:

```shell
pip install pytest
```

Після цього ви можете написати свої тести простішим способом, без потреби у створенні підкласів:

```python
# Збережіть це у файлі під назвою test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Має бути 12"
```

Щоб запустити ваші тести з `pytest`, просто виконайте:

```shell
pytest test_with_pytest.py
```

Ви повинні побачити вихідні дані від pytest, що показують результати вашого тестування.
