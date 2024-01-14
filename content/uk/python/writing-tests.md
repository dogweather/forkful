---
title:                "Python: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Написання тестів є важливою частиною процесу програмування, оскільки допомагає забезпечити якість та надійність коду. Це дозволяє виявити помилки та баги раніше, тим самим економлячи час та ресурси при налагодженні програми. Крім того, написання тестів допомагає уникнути недоліків та зберегти час у майбутньому.

## Як написати тести в Python

Нижче наведені декілька прикладів кодів в мові Python, які допоможуть зрозуміти процес написання тестів.

```Python
# Імпортування необхідних бібліотек
import unittest

# Створення класу тестів
class TestStringMethods(unittest.TestCase):

    # Перевірка функції lower()
    def test_lower(self):
        self.assertEqual('HELLO'.lower(), 'hello')

    # Перевірка функції isupper()
    def test_isupper(self):
        self.assertTrue('HELLO'.isupper())
        self.assertFalse('Hello'.isupper())

    # Перевірка методу join()
    def test_join(self):
        s = ','
        self.assertEqual(s.join(['a', 'b', 'c']), 'a,b,c')

# Запуск тестів
if __name__ == '__main__':
    unittest.main()
```

Виведення на консоль:

```
Ran 3 tests in 0.000s

OK
```

В даному прикладі ми використовуємо бібліотеку `unittest`, яка надає нам можливість створювати тестові набори за допомогою класу `TestCase`. За допомогою методу `assertEqual()` ми перевіряємо, чи дорівнюють значення виразів між собою. Методи `assertTrue()` та `assertFalse()` перевіряють співставність булевих значень. Також, ми можемо перевіряти не лише підмодулі строки, а й інші вбудовані типи даних.

## Огляд написання тестів

Написання тестів допомагає розбити програму на більш малий код, який можна легше тестувати. Це також збільшує перевірку коду та полегшує налагодження проблем, якщо такі з'являться.

Існує багато інструментів для тестування в мові Python, такі як `unittest`, `pytest`, `doctest` та інші. Крім того, робота з ними дозволяє швидше розробляти програми та уникати помилок.

## Дивіться також

- [Документація по unittest в Python](https://docs.python.org/3/library/unittest.html)
- [Підручник по тестуванню коду в Python](https://realpython.com/python-testing/)
- [Огляд основних інструментів для тестування в Python](https://towardsdatascience.com/testing-in-python-6b56ed9c689d)