---
title:                "Перетворення рядка до нижнього регістру"
html_title:           "Swift: Перетворення рядка до нижнього регістру"
simple_title:         "Перетворення рядка до нижнього регістру"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що & Чому?
Конвертування рядка до нижнього регістру означає перетворення всіх літер у рядку на їхні еквіваленти у нижньому регістрі. Це корисно для порівняння рядків, а також для використання рядків у методах алгоритмів, де регістр літер не має значення.

## Як зробити:
```Swift
let exampleString = "Hello World!"
let lowerCaseString = exampleString.lowercased()
print(lowerCaseString) // виведе "hello world!"
```

## Глибокий погляд:
Історичний контекст: у різних мов програмування існує різні методи перетворення рядків до нижнього регістру. У Swift це робиться за допомогою методу `lowercased()`. 

Альтернативи: у деяких мовах програмування є окремий тип даних "рядок в нижньому регістрі", який зберігає рядок вже в нижньому регістрі. Таким чином, не потрібно перетворювати його перед використанням.

Деталі реалізації: при конвертуванні рядка до нижнього регістру, Swift використовує правила Unicode для перетворення букв. Наприклад, "Ü" буде перетворено в "ü".

## Дивись також:
- [Рядки і регістр в Swift](https://swiftbook.ru/content/languageguide/strings-and-characters/) (рос.)
- [Unicode і регістр літер](https://www.unicode.org/faq/casemap_charprop.html) (англ.)