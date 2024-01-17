---
title:                "Створення випадкових чисел"
html_title:           "Swift: Створення випадкових чисел"
simple_title:         "Створення випадкових чисел"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що та чому?
Створення випадкових чисел (generating random numbers) - це процес отримання чисел, які не мають конкретної послідовності або логічного походження. Програмісти використовують цей процес для різноманітних задач, таких як генерація службових паролів, випадкового вибору елементів для гри або тестування програм.

## Як це зробити:
```
func generateRandomNumber() -> Int {
    return Int.random(in: 1...10)
}

let randomNumber = generateRandomNumber()
print("Ваше випадкове число: \(randomNumber)")
```
В результаті отримаємо випадкове число від 1 до 10. Можна також змінити діапазон за допомогою змінних замість конкретних чисел.

## Глибокий погляд:
Спочатку для генерації випадкових чисел програмісти використовували різні формули і алгоритми. Проте, вони не завжди були ефективними або необхідними для даної задачі. З появою спеціальної функції Int.random, яка є частиною мови Swift, процес став більш швидким та простим. У разі, якщо вам потрібні більш точні випадкові числа, можна використовувати спеціальні бібліотеки, такі як GameKit.

## Дивіться також:
- [Документація по Int.random](https://developer.apple.com/documentation/swift/int/random)
- [Стаття про випадкові числа на SwiftBySundell](https://www.swiftbysundell.com/articles/random-numbers-in-swift/)