---
title:    "Swift: Пошук та заміна тексту."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

У пошуку та заміні тексту є важлива роль в програмуванні на Swift. Воно дозволяє замінювати певні частини тексту швидко та ефективно, що робить процес програмування менш часозатратним та швидким.

## Як

Для початку, спробуйте використати метод `replacingOccurrences(of:with:)`, який замінює всі входження певної підстрічки на іншу підстрічку.

```Swift
let message = "Сьогодні день народження, щасливого дня!"
let newMessage = message.replacingOccurrences(of: "народження", with: "весілля")
print(newMessage)

// поточне значення message: Сьогодні день народження, щасливого дня!
// нове значення newMessage: Сьогодні день весілля, щасливого дня!
```

Також, можна використовувати метод `components(separatedBy:)`, який розбиває рядок на масив підрядків окремою підстрічкою.

```Swift
let message = "Сьогодні день народження, щасливого дня!"
let words = message.components(separatedBy: " ")
print(words)

// вивід: ["Сьогодні", "день", "народження,", "щасливого", "дня!"]
```

## Deep Dive

При пошуку та заміні тексту, важливо звертати увагу на регістр символів. Наприклад, якщо потрібно замінити слово "день" на "ніч", але в рядку є слово "День", то воно залишиться незмінним, оскільки символ "д" не збігається з символом "Д". Щоб уникнути цієї проблеми, можна використовувати метод `replacingOccurrences(of:with:options:)` та встановити параметр `options` як `.caseInsensitive`, щоб ігнорувати регістр символів.

```Swift
let message = "День народження, щасливого дня!"
let newMessage = message.replacingOccurrences(of: "день", with: "ніч", options: .caseInsensitive)
print(newMessage)

// вивід: Ніч народження, щасливого дня!
```

## See Also

- [Документація Apple для методу `replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/swift/string/1642143-replacingoccurrences)
- [Документація Apple для методу `components(separatedBy:)`](https://developer.apple.com/documentation/swift/string/1641697-components)