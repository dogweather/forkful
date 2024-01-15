---
title:                "Пошук та заміна тексту."
html_title:           "Swift: Пошук та заміна тексту."
simple_title:         "Пошук та заміна тексту."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Варіація тексту є важливим аспектом програмування, і іноді вам може знадобитися замінити певні фрази або символи у вашому коді. Це може бути зручно під час великих проектів, коли необхідно виконати однакові зміни в багатьох місцях.

## Як

У Swift є потужний інструмент для пошуку та заміни тексту - `string.replacingOccurrences(of:with:)`. Для його використання потрібно передати два аргументи - фразу чи символ, який потрібно замінити, та нову фразу чи символ, яким буде замінено старий. Наприклад:

```Swift
let text = "Hello, World!"
let newText = text.replacingOccurrences(of: "Hello", with: "Hi")
print(newText) // Виведе "Hi, World!"
```

Цей метод також можна використовувати для заміни певних символів, наприклад, заміна розділових знаків на пробіли:

```Swift
let numbers = "1, 2, 3, 4, 5"
let newNumbers = numbers.replacingOccurrences(of: ",", with: " ")
print(newNumbers) // Виведе "1 2 3 4 5"
```

Є також можливість використовувати регулярні вирази для пошуку і заміни тексту за певних умов. Наприклад, аргумент `options` можна використовувати для визначення, чи регулярний вираз повинен враховувати регістр символів:

```Swift
let text = "Hello, World!"
let newText = text.replacingOccurrences(of: "hello", with: "Hi", options: [.caseInsensitive])
print(newText) // Виведе "Hi, World!"
```

Існує багато інших опцій та можливостей для пошуку та заміни тексту у Swift. Для більш детальної інформації звертайтеся до документації.

## Глибокий погляд

Метод `string.replacingOccurrences(of:with:)` є частиною класу `String`, який надає багато інших корисних функцій для роботи з текстом. Наприклад, можна використовувати метод `contains(_:)` для перевірки наявності певної фрази чи символу в тексті, або метод `split` для розділення рядка на масив за певним роздільником.

Також важливо зазначити, що метод `string.replacingOccurrences(of:with:)` повертає новий рядок, а не змінює оригінальний. Це означає, що при зміні тексту потрібно присвоїти нове значення змінній, а не прямо до тексту, який хочете змінити.

## Див. також

- [Документація Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Туторіал по пошуку та заміні тексту в Swift](https://www.hackingwithswift