---
title:    "Swift: Шукаючи довжину рядка"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

Існує багато ситуацій у програмуванні, коли потрібно визначити довжину рядка, наприклад, для перевірки валідності введених даних або для виконання певних дій залежно від кількості символів у рядку. Тому вміння знайти довжину рядка є важливим для будь-якого розробника.

## Як це зробити

Для визначення довжини рядка використовується вбудована функція `count` у Swift. Для цього потрібно передати рядок, довжину якого потрібно знайти, у цю функцію. Нижче наведений приклад коду:

```Swift
let string = "Привіт, світе!"
print(string.count) // Виведе 14, оскільки рядок складається з 14 символів.
```

Ви також можете використовувати функцію `characters.count`, яка виконує ту саму функцію, але була видалена у Swift 4.

```Swift
let string = "Привіт, світе!"
print(string.characters.count) // Виведе 14.
```

## Глибше

Рядковий тип даних в Swift включає у себе сутність, яка називається `String.Index`, яка використовується для відстеження позиції в рядку. Це дозволяє використовувати функцію `count` у рядку з будь-якою локалізацією та символами, зокрема емодзі. Крім того, існує можливість використання як літер, так і байтів для визначення довжини рядка.

## Дивіться також

- [The Swift Programming Language - String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Apple Developer Documentation - Strings and Characters](https://developer.apple.com/documentation/swift/strings_and_characters)