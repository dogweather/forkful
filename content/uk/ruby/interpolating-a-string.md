---
title:                "Інтерполяція рядка"
html_title:           "Ruby: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що & Чому?
Інтерполяція рядків - це коли ми можемо використовувати змінні в середині рядків та отримувати бажане значення. Програмісти використовують інтерполяцію рядків для зручності та ефективності, оскільки не потрібно додавати зайві кроки для об'єднання різних значень у рядку.

## Як:
```Ruby
name = "Оксана"
age = 25

puts "Привіт, мене звати #{name} і мені #{age} років."
# Виведе: Привіт, мене звати Оксана і мені 25 років.
```

## Глибока занурення:
Інтерполяція рядків була додана в Ruby вже в першій версії у 1993 році. Попередні методи, такі як використання оператора "+" для об'єднання рядків, можуть бути менш зрозумілими та більш лінійними. Однак, при використанні багатьох змінних чи складних рядків, інтерполяція може бути більш зручним і зрозумілим варіантом.

## Дивіться також:
- [Документація Ruby про інтерполяцію рядків](https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-String+Interpolation)
- [Туторіал "Рубі з нуля"](https://rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/18-strings/lessons/84-string-interpolation)
- [Стаття на Medium про інтерполяцію рядків в Ruby](https://medium.com/@bparanj/interpolation-string-versus-adding-strings-in-ruby-a294c7e1a16b)