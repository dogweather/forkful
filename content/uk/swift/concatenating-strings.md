---
title:                "Swift: З'єднання рядків"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Конкатенація рядків є важливим елементом в програмуванні мовою Swift. Вона дозволяє об'єднувати різні рядки для створення більш складних повідомлень або даних. Це особливо корисно, коли ми працюємо зі змінними, що містять текстову інформацію.

## Як це зробити

Для конкатенації рядків ми використовуємо оператор "+" або метод "concatenate" для акумулювання рядків. Давайте подивимося на кілька прикладів:

```Swift
// Використання оператора "+"
let firstName = "Андрій"
let lastName = "Петренко"
let fullName = firstName + lastName
print(fullName) // Виведе "АндрійПетренко"

// Використання методу "concatenate"
let age = 28
let message = "Мені " + String(age).concatenate(" років")
print(message) // Виведе "Мені 28 років"
```

У першому прикладі ми з'єднали дві змінні типу String, а в другому - змінну типу Int з рядком. У обох випадках ми отримали новий рядок, який містить об'єднану інформацію. 

## Глибше в деталі

Оператор "+" також можна використовувати для додавання більш ніж двох рядків. Також можна використовувати "concatenate" метод для кількох рядків одночасно. Наприклад:

```Swift
let login = "user"
let domain = "example"
let extension = "com"
let email = login + "@" + domain + "." + extension
print(email) // Виведе "user@example.com"

let welcome = String.concatenate("Вітаємо, ", "андрій", "!", "!") 
print(welcome) // Виведе "Вітаємо, андрій!!"
```

Також важливо пам'ятати, що оператор "+" можна використовувати не тільки для рядків, але і для інших типів даних, таких як числа або булеві значення. За допомогою нього можна створювати складніші вирази і отримувати більш різноманітні результати.

## Дивись також

- [Офіційна документація Swift про роботу з рядками](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Ресурси для навчання програмування Swift](https://www.apple.com/swift/resources/)
- [Інша стаття про роботу з рядками в Swift](https://medium.com/swift-programming/conquering-strings-in-swift-32b81d5accc)