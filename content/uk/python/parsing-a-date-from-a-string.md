---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що й чому?
Аналіз дати з рядка - це процес, коли ми витягуємо інформацію про дату з текстового рядка. Програмісти роблять це, щоб спростити обробку дати та часу за допомогою програми.

## Як це зробити:
У Python для парсинга дат з рядків ми використовуємо модуль `datetime`. Ось простий приклад:

```Python
from datetime import datetime

data_str = '12/10/2021'

data_obj = datetime.strptime(data_str, '%d/%m/%Y')

print(data_obj)
# 2021-10-12 00:00:00
```

У цьому коді ми перетворюємо текстовий рядок '12/10/2021' на об’єкт datetime.

## Поглиблений аналіз
Перетворення рядків на дати було важливою частиною комп'ютерного програмування практично з початку його існування. Історично, дати зберігалися як текст, що призвело до потреби їх аналізу.

Є інші способи парсінгу дат з рядків у Python, наприклад, можна використовувати модуль `dateutil`, який набагато гнучкіше інтерпретує вхідний текстовий рядок.

Слід зазначити, що при аналізі даних залежно від формату можуть виникнути різного роду помилки та непридатні дані для аналізу.

## Додатково
- Документація по Python `datetime` модулю: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
- Документація по Python `dateutil` модулю: [https://dateutil.readthedocs.io/en/stable/](https://dateutil.readthedocs.io/en/stable/)