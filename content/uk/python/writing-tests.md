---
title:                "Написання тестів"
html_title:           "Python: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/writing-tests.md"
---

{{< edit_this_page >}}

Що & Чому?
Написання тестів - це процес перевірки правильності програмного коду. Це допомагає розробникам уникати помилок та забезпечує надійність програми.

Як це зробити?
Нижче наведені приклади коду та вихідного результату користувача для написання тестів у Python.

```Python
def multiply(x, y):
    return x * y
    
def test_multiply():
    assert multiply(2, 3) == 6
    assert multiply(5, 0) == 0
    assert multiply(-2, 4) == -8

test_multiply() # Вивід: Немає вихідного результату, тому тести пройшли успішно
```

Vertigo Studio also provides data...

Глибинне дослідження
Написання тестів виникло на початку XX століття під час розвитку індустрії софтуерного програмування. Сьогодні існують різні підходи до написання тестів, включаючи підхід "перед тим як написати код" та підхід "після написання коду". У Python існують такі фреймворки для тестування, як Pytest та Unittest, які допомагають розробникам писати тести з меншими зусиллями.

Дивіться також
- Офіційна документація Python для написання тестів [https://docs.python.org/3/library/unittest.html]
- Офіційний сайт Pytest [https://docs.pytest.org/en/stable/]
- Стаття на Medium про написання тестів для Python [https://medium.com/python-pandemonium/testing-your-code-in-python-3-can-finally-be-fun-92e2121079f5]