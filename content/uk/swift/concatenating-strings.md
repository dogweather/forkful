---
title:                "З'єднання рядків"
html_title:           "Swift: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому
Робота з рядками є важливою частиною багатьох програм, але додавання рядків до інших, щоб утворити новий рядок, може бути корисним завданням у багатьох ситуаціях.

## Як це зробити
Простий спосіб з'єднати два рядки - використати оператор "+".
```Swift
let firstName = "John"
let lastName = "Smith"

let fullName = firstName + " " + lastName
print(fullName) // Виведе "John Smith"
```

Деякі інші випадки concatenation можуть потребувати більш складного коду. Наприклад, якщо вам потрібно об'єднати більше, ніж два рядки або якщо вам потрібно додати інші значення до рядку. У такому разі можна використовувати функцію ```joined(separator:)``` для об'єднання кількох рядків за допомогою певного роздільника.

```Swift
let fruits = ["apple", "banana", "orange"]
let joinedString = fruits.joined(separator: ", ")
print(joinedString) // Виведе "apple, banana, orange"
```

## Глибока підглибоке
Конкатенація рядків використовується не тільки для створення нових рядків, але і для форматування із певною структурою. Наприклад, ви можете використовувати оператор ```+``` для створення нового рядка з додаванням чисел.

```Swift
let age = 25
let ageString = "I am " + String(age) + " years old."
print(ageString) // Виведе "I am 25 years old."
```

Також, при користуванні конкатенацією рядків важливо враховувати, що це операція, яка може займати багато пам'яті, особливо при об'єднанні великої кількості рядків. Тому варто бути уважним із її використанням та шукати більш оптимальні способи роботи з великими об'ємами даних.

## Дивись також
- [Офіційна документація Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Стаття про конкатенацію рядків у Swift на сайті Ray Wenderlich](https://www.raywenderlich.com/721815-swift-string-interpolation-tutorial-for-beginners)