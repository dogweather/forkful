---
title:                "Капіталізація рядка"
html_title:           "Python: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Існує багато різних випадків, коли нам потрібно зробити рядок великими літерами. Наприклад, коли ми отримуємо ім'я користувача з його реєстраційної форми, ми хочемо, щоб воно було коректно відображене на сторінці вітання. Або коли ми хочемо вивести заголовок у капіталізованому вигляді. Також, іноді важко читати рядки, написані у нижньому регістрі. Тому має сенс знати, як капіталізувати рядок у Python.

## Як

```python
my_string = "welcome to python"
print(my_string.capitalize())
```

Результат:
```python
Welcome to python
```

## Deep Dive

У Python існує кілька різних методів для капіталізації рядка. Один з них - використання методу `capitalize()`, який ми використовували у прикладі вище. Цей метод переводить першу букву рядка у верхній регістр, а всі інші - у нижній. Також, є метод `upper()`, який капіталізує всі літери у рядку. І метод `title()`, який капіталізує першу букву кожного слова у рядку.

## See Also

- [Документація Python про рядки](https://docs.python.org/3/tutorial/introduction.html#strings)
- [Туторіал по роботі з рядками у Python](https://www.programiz.com/python-programming/string)