---
title:                "Перетворення рядка у нижній регістр"
aliases:
- uk/python/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:59.795827-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Переведення рядків у нижній регістр – це зміна всіх великих літер на малі в текстовому рядку. Програмісти це роблять для уніфікації даних, полегшення порівнянь та пошуку.

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
