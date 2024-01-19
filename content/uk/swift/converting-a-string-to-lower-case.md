---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення рядків на нижній регістр - це процес заміни всіх великих літер на маленькі в рядку. Це використовується програмістами для нормалізації даних, наприклад, при порівнянні рядків або перевірці контексту.

## Як це зробити:

Використовуючи Swift, ми можемо легко змінити рядок до нижнього регістру. Ось як:

```Swift
var str = "HELLO, WORLD!"
str = str.lowercased()
print(str)
```

Виходом буде:

```
hello, world!
```

## Пірнемо глибше:

1. **Історичний контекст:** Функція переведення рядків до нижнього регістру була однією з перших високорівневих функцій обробки рядків, що з'явилися в ранніх мовах програмування.

2. **Альтернативи:** Іноді, замість того, щоб переводити весь рядок в нижній регістр, ви можете вирішити перевести лише першу букву рядка до верхнього регістру (так званий "Title Case"). Це також можливо в Swift:

```Swift
var str = "hello, world!"
str = str.capitalized
print(str)
```

Виведення:

```
Hello, World!
```

3. **Деталі реалізації:** Функція `.lowercased()` в Swift використовує Unicode для точного визначення, як перетворювати кожну букву. Це означає, що вона працює із символами з різних мов та скриптів, не тільки з ASCII.

## Дивись також:

1. [String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) - Офіційний підручник Swift про рядки та символи.
2. [Unicode](https://unicode.org) - Сайт Unicode, де ви можете дізнатися більше про те, як працюють символи в комп'ютерах.
3. [Swift API Reference](https://developer.apple.com/documentation/swift/string) - Опис функції `.lowercased()` в документації Apple.