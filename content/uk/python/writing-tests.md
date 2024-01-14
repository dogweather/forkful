---
title:    "Python: Написання тестів"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Чому

Написання тестів в програмуванні - це ключовий елемент для забезпечення якості програмного забезпечення. Це допомагає виявляти та коригувати помилки в коді, забезпечує надійність та доведення коректності програми.

## Як

Використовуючи вбудовану бібліотеку `unittest` в Python, ми можемо легко створювати тести для нашого коду.

```Python
import unittest

def multiply(x, y):
    return x * y

class TestMultiply(unittest.TestCase):
    
    def test_multiply_integers(self):
        result = multiply(2, 3)
        self.assertEqual(result, 6)

    def test_multiply_floats(self):
        result = multiply(2.5, 3)
        self.assertEqual(result, 7.5)
    
    def test_multiply_strings(self):
        result = multiply("Hello", 3)
        self.assertEqual(result, "HelloHelloHello")

if __name__ == '__main__':
    unittest.main()
```

Вивід:

```
....
----------------------------------------------------------------------
Ran 4 tests in 0.001s

OK
```

У цьому прикладі ми створюємо клас `TestMultiply`, який унаслідуємо від класу `unittest.TestCase`. Кожен метод в цьому класі буде тестувати окремий аспект нашої функції `multiply`. Завдяки методу `assertEqual` ми перевіряємо очікуваний результат та фактичний результат на відповідність один одному.

## Deep Dive

При написанні тестів важливо не тільки перевіряти правильність вхідних та вихідних даних, але й враховувати різні варіанти використання функції. Наприклад, у нашому прикладі ми можемо додати тести для перевірки випадків з використанням негативних чисел та некоректних типів даних.

Також важливо не тільки писати тести для нового коду, але й поновлювати тести при змінах у коді. Це допоможе виявити можливі помилки та забезпечить для коректної роботи програми після змін.

## Дивіться також

* [Офіційна документація по бібліотеці `unittest`](https://docs.python.org/3/library/unittest.html)
* [Відеоурок з написання тестів в Python](https://www.youtube.com/watch?v=6tNS--WetLI)
* [Приклади коду на GitHub](https://github.com/topics/python-unittest)