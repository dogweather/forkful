---
title:                "Swift: Видобуття підрядків"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Чому

Використання витягнених підрядків дозволяє зробити ваш код більш гнучким і працездатним, дозволяючи вам обробляти лише необхідну частину рядка замість повного рядка.

## Як

```Swift
let fullName = "Ivan Kovalenko"
let firstName = fullName.prefix(4)
print(firstName) // Output: "Ivan"
```

Цей приклад використовує метод `prefix` для вилучення перших 4 символів зі стрічки `fullName`.

```Swift
let sentence = "Сьогодні день народження"
let month = sentence.dropFirst(8)
print(month) // Output: "день"
```

У цьому випадку ми використовуємо метод `dropFirst` для вилучення перших 8 символів зі стрічки `sentence`, залишаючи тільки слово "день".

## Глибоке дослідження

Методи `prefix` та `dropFirst` можна використовувати не тільки з числами, але й зі змінними та константами типу `String.Index` для більшого контролю над витягненими підрядками.

Наприклад, ви можете використовувати метод `index` для отримання конкретного символу у рядку та використовувати його як аргумент для методів `prefix` та `dropFirst`.

```Swift
let name = "Оксана"
let index = name.index(name.startIndex, offsetBy: 3)
let lastTwoLetters = name.suffix(from: index)
print(lastTwoLetters) // Output: "на"
```

У цьому прикладі ми використовуємо метод `index` для отримання індексу третього символу стрічки `name`, а потім використовуємо цей індекс як аргумент для методу `suffix` для отримання останніх двох літер.

##Дивись також

- [Офіційна документація Swift для методу prefix](https://developer.apple.com/documentation/swift/string/1643039-prefix)
- [Офіційна документація Swift для методу dropFirst](https://developer.apple.com/documentation/swift/string/1642945-dropfirst)
- [Ресурси для вивчення Swift на Українській мові](https://swiftbook.org.ua)