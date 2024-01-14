---
title:                "Swift: Капіталізація рядка"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Чому

Капіталізація рядка є важливою частиною програмування, оскільки це дозволяє змінювати вигляд тексту, що відображається користувачам. Наприклад, це може бути корисно для виведення заголовків, імен чи інших важливих даних з великими літерами для кращого читання.

# Як

Кодування для капіталізації рядка дуже просте в мові програмування Swift. Для цього можна використовувати вбудовану функцію `uppercased()` для перетворення рядка в верхній регістр. Наприклад:

```Swift
let str = "привіт"
print(str.uppercased())
```

Вивід: `ПРИВІТ`

# Глибинний аналіз

Справжнє застосування капіталізації рядка стає очевидним, коли ми починаємо глибше досліджувати її функціональність. Наприклад, в додатку часто потрібно змінювати тільки перше слово рядка на верхній регістр, а решта залишати без змін. Для цього можна використовувати функцію `prefix(1)` для отримання першої літери рядка та функцію `uppercased()` для капіталізації цієї літери. Наприклад:

```Swift
let str = "привіт друзі"
let firstLetter = str.prefix(1).uppercased()
let restOfString = str.dropFirst()

print("\(firstLetter)\(restOfString)")
```

Вивід: `Привіт друзі`

# Дивись також

- [The Swift Programming Language - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift by Sundell - Working with strings in Swift](https://ww