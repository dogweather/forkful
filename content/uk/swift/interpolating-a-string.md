---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Інтерполяція рядків - це техніка, що дозволяє вставляти значення змінних прямо в текстові рядки. Програмісти використовують її для будівництва найбільш читабельних і динамічних рядків.

## Як це працює:

```Swift
var name = "Віктор"
print("Привіт, \(name)")
```

Це виведе "Привіт, Віктор"

А якщо ми змінимо значення змінної `name`, рядок автоматично оновиться:

```Swift
name = "Наташа"
print("Привіт, \(name)")
```

Це виведе "Привіт, Наташа"

## Більше деталей

Інтерполяція рядків була запозичена у Swift з інших мов програмування, таких як Perl і Ruby. Альтернативами цьому є синтаксис форматування рядків (як і в Python) або конкатенація рядків, але інтерполяція рядків є більш компактноюта зручною.

Що стосується деталей імплементації, інтерполяція рядків в Swift використовує функцію `description` протоколу `CustomStringConvertible`, що дозволяє вставляти різні типи даних без необхідності перетворення їх на рядки.

## Більше інформації

Можна знайти на [офіційному сайті Swift](https://swift.org/documentation/#the-swift-programming-language), [документації Apple про інтерполяцію рядків](https://developer.apple.com/documentation/swift/string_interpolation) та на [Swift by Sundell](https://www.swiftbysundell.com/posts/string-interpolation-in-swift)