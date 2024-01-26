---
title:                "Перетворення рядка на великі літери"
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
У Python приведення рядків до великих літер — це зміна всіх символів тексту на великі. Програмісти це роблять для нормалізації введення або щоб забезпечити візуальну послідовність у текстових даних.

## Як це зробити:
```Python
# Приклад використання функції upper() для капіталізації рядка
text = "привіт світ"
capitalized_text = text.upper()
print(capitalized_text)  # Виведе: 'ПРИВІТ СВІТ'

# Приклад капіталізації лише першої літери
title_text = text.capitalize()
print(title_text)  # Виведе: 'Привіт світ'
```

## Поглиблений розгляд:
Ключові функції для роботи з текстом у Python беруть свій початок ще з ранніх версій мови. Метод `.upper()` існує практично у всіх стрічкових об'єктах, що дозволяє легко перетворювати рядки на верхній регістр. Альтернативою `.upper()` є `.capitalize()`, який робить лише першу літеру великою, та `.title()`, який приводить до великої літери перші букви всіх слів. Під капотом ці методи працюють з Unicode таблицею, перевіряючи та замінюючи символи відповідно до їх кодів.

## Дивіться також:
- Документація по рядковим методам у Python: https://docs.python.org/3/library/stdtypes.html#string-methods
- Unicode стандарт для символів у Python: https://www.unicode.org/standard/standard.html
- Рекомендації по стилю написання коду у Python, PEP 8: https://www.python.org/dev/peps/pep-0008/
