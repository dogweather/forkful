---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:38:17.438534-07:00
simple_title:         "Аналіз дати з рядка"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке парсинг дат та чому це важливо?
Parsing a date from a string involves converting text to a date object. We do this to manipulate dates easily – compare, store, or change formats.

## How to:
```Python
from datetime import datetime

# Створюємо строку з датою
date_string = "14.04.2023"

# Парсимо строку у datetime об'єкт
date_object = datetime.strptime(date_string, '%d.%m.%Y')

print(date_object)  # Output: 2023-04-14 00:00:00
```

```Python
# Зміна формату дати
formatted_date = date_object.strftime('%Y/%m/%d')
print(formatted_date)  # Output: 2023/04/14
```

```Python
# Обробка неправильного формату
try:
    wrong_date_string = "2023-14-04"
    datetime.strptime(wrong_date_string, '%Y-%d-%m')
except ValueError as e:
    print(e)  # Output: time data '2023-14-04' does not match format '%Y-%d-%m'
```

## Deep Dive
Parsing dates is about understanding. It started with manual parsing - think splitting strings and converting to integers. Today, libraries like `datetime` in Python handle the complexity.

Alternatives? Yes, many. `dateutil` is smarter at guessing formats. For web apps, libraries like `date-fns` or `Moment.js` (JavaScript) are common.

Implementation details? The `strptime` method follows patterns (directives) like `%Y` for the year. It raises `ValueError` if things don't match. It's all about the rules you set.

## See Also
- Python `datetime` documentation: https://docs.python.org/3/library/datetime.html
- `dateutil` parser: https://dateutil.readthedocs.io/en/stable/parser.html
- For globalization and localization: https://babel.pocoo.org/
- Python `strftime` directives: http://strftime.org/
