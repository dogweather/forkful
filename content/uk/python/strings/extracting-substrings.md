---
title:                "Виділення підрядків"
aliases:
- /uk/python/extracting-substrings/
date:                  2024-01-20T17:46:31.835873-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Витягування підрядків — це процес отримання частин рядків з більших. Програмісти роблять це для аналізу тексту, обробки даних, або просто щоб витягти значущу інформацію.

## How to: (Як це зробити:)
```Python
text = "Привіт, світ!"
# Витягнемо слово "Привіт"
substring = text[0:6]
print(substring) # Виведе: Привіт

# Витягнемо слово "світ"
another_substring = text[8:12]
print(another_substring) # Виведе: світ

# Робота зі зворотними індексами
reverse_substring = text[-5:-1]
print(reverse_substring) # Виведе: світ

# Використання методу slice()
slice_substring = text[slice(8, 12)]
print(slice_substring) # Виведе: світ
```

## Deep Dive (Поглиблений занурення)
Витягування підрядків — частина стандартного багажу мови Python від початку. 

Python використовує індексацію з нуля, що означає, що перший символ рядка має індекс 0. Підрядок можна отримати за допомогою синтаксису `[start:end]`, де `start` включно, а `end` виключно. 

Є концепти такі як "зворотний індекс" (де від'ємне число -1 вказує на останній символ), що спрощує роботу з кінця рядка.

Існують інші методи, такі як `slice()` і розширені синтаксиси з `start:end:step`, але базовий `[start:end]` часто достатньо і читабельніше для початківців.

Історично, витягування підрядків було завжди важливим у програмуванні, особливо коли трактуємо форматований або неструктурований текст. 

Також існує множина методів об'єктів рядків як `startswith()`, `endswith()`, `find()`, `index()`, які можуть допомогти в пошуку та аналізі підрядків.

## See Also (Дивіться також)
- Документація Python про рядки: https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str
- Розуміння Python String Slicing: https://realpython.com/python-string-slicing/
- Ефективність та вибір методів обробки рядків: https://www.afternerd.com/blog/python/strings/
