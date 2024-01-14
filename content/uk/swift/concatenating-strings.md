---
title:                "Swift: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Конкатенація рядків є корисною у програмуванні, оскільки дозволяє об'єднати два або більше рядки в один. Це особливо корисно при створенні повідомлень, логів, або будь-якого виведення користувачу.

## Як

Для конкатенації рядків у Swift можна використовувати оператор `+`, або метод `joined(separator:)`. Приклади коду і відповідний результат показані нижче:

```Swift
// Використання оператора +
let myName = "Оксана"
let myLastName = "Коваленко"
let fullName = myName + " " + myLastName
print(fullName) // Результат: Оксана Коваленко

// Використання методу joined(separator:)
let hobbies = ["малювання", "гра на гітарі", "подорожі"]
let allHobbies = hobbies.joined(separator: ", ")
print(allHobbies) // Результат: малювання, гра на гітарі, подорожі
```

## Огляд

При використанні оператора `+`, Swift фактично створює новий рядок, який містить з'єднання обох вхідних рядків. Тому цей метод не є найкращим рішенням для об'єднання багатьох рядків, оскільки це може призвести до зайвого використання пам'яті. Метод `joined(separator:)` з'єднує рядки за допомогою вказаного роздільника, що робить його ефективнішим для об'єднання багатьох рядків одночасно.

## Дивіться також

- [Документація Apple про конкатенацію рядків у Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Розділ про конкатенацію рядків у курсі "Основи Swift"](https://www.udemy.com/course/swiftbeginner/learn/lecture/23397050?start=180#overview)