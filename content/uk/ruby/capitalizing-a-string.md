---
title:                "Капіталізація стрічки"
html_title:           "Ruby: Капіталізація стрічки"
simple_title:         "Капіталізація стрічки"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Написання тільки великої літери на початку рядка - це процес, який використовують програмісти для зміни форматування тексту. Це зручний спосіб надати видільність слову або фразі в рядку.

## Як це зробити:
```
"hello world".capitalize 
 # Output: "Hello world"

"ruby programming".capitalize 
 # Output: "Ruby programming"

"123abc".capitalize 
 # Output: "123abc"
```

## Глибокий занурення:
Написання тільки великої літери використовується вже з давніх часів для виділення важливих слів у тексті. Існують інші методи форматування тексту, такі як використання HTML тегів. У Ruby є також методи `upcase` і `capitalize!` для зміни регістру всього рядка або його першої літери після призначення зміній.

## Подивіться також:
- [Документація Ruby з методом `capitalize`](https://ruby-doc.org/core-2.7.2/String.html#method-i-capitalize)
- [Спробуйте використати CamelCase для імен змінних](https://github.com/bbatsov/ruby-style-guide#naming)