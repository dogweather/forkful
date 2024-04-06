---
date: 2024-01-20 17:38:59.795827-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) \u0429\u0435 \u043F\u0440\u0438\u043A\u043B\u0430\u0434, \u043A\u043E\u043B\
  \u0438 \u043F\u043E\u0440\u0456\u0432\u043D\u044E\u0454\u043C\u043E \u0440\u044F\
  \u0434\u043A\u0438."
lastmod: '2024-04-05T21:53:48.830152-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ \u0429\u0435 \u043F\u0440\u0438\u043A\u043B\u0430\u0434, \u043A\u043E\u043B\u0438\
  \ \u043F\u043E\u0440\u0456\u0432\u043D\u044E\u0454\u043C\u043E \u0440\u044F\u0434\
  \u043A\u0438."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 4
---

## How to: (Як це зробити:)
```Python
# Перетворення рядка у нижній регістр
text = "Привіт, Як справи?"
lower_text = text.lower()

print(lower_text)  # виведе: привіт, як справи?
```

Ще приклад, коли порівнюємо рядки:
```Python
# Порівняння рядків без урахування регістра
user_input = "Київ"
city = "київ"

print(user_input.lower() == city.lower())  # виведе: True
```

## Deep Dive (Поглиблений розбір)
Python's `str.lower()` method dates back to the early versions of Python. It allows strings to be converted to a lower case, which is especially useful because case sensitivity can lead to issues during string comparisons. This feature is not unique to Python: most programming languages offer similar functionality.

Alternatives include using regular expressions or manual mapping of characters to their lower-case equivalents, but these methods are more verbose and error-prone.

Under the hood, `str.lower()` works by iterating over each character in the string and mapping it to its lower-case equivalent based on Unicode standard. This means it can correctly handle most languages, including complex cases like German's ß, which becomes 'ss'.

## See Also (Дивіться також)
- The official Python documentation on string methods: https://docs.python.org/3/library/stdtypes.html#string-methods
- Unicode Standard for characters: https://home.unicode.org/
- Python PEP regarding string methods and Unicode: https://www.python.org/dev/peps/pep-3137/
