---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Коли мова йде про форматування рядків у Swift, однією з базових операцій є перетворення всіх букв на великі, тобто "capitalization". Це корисно для єдності відображення тексту, при введенні даних користувачами або для візуального виділення розділів тексту.

## Як це зробити:
```Swift
let lowerCaseString = "це приклад рядка."
let capitalizedString = lowerCaseString.capitalized

print(capitalizedString)  // Вивід: "Це Приклад Рядка."
```

Для трансформації в усі великі літери:
```Swift
let upperCaseString = lowerCaseString.uppercased()

print(upperCaseString)  // Вивід: "ЦЕ ПРИКЛАД РЯДКА."
```

## Поглиблено:
Capitalization у програмуванні не новий тренд. Вперше вона з'явилась ще у мовах програмування, які працювали на великих комп'’ютерних системах. У Swift, `capitalized` є властивістю рядка, яка перетворює першу літеру кожного слова в рядку на велику. У випадку з `uppercased()`, метод повертає новий рядок, де всі літери є великими. 

Варто знати, що `capitalized` у Swift може працювати не так, як ви очікуєте, з нелатинськими алфавітами. Наприклад, українське “і” переміщується до верхнього регістру не так очевидно, як у англійській мові. Тому перевіряйте результати ретельно.

Альтернативою `capitalized` та `uppercased()` може бути власна реалізація з використанням функціоналу мови, наприклад, мапування рядка із заданими умовами.

## Дивіться також:
- Документацію по строковим типам даних в Swift: [Swift String](https://developer.apple.com/documentation/swift/string)
- Аrticle on Unicode and text processing: [Unicode.org](http://unicode.org/)
- The Swift Programming Language Guide to Strings and Characters: [Swift Guide](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
