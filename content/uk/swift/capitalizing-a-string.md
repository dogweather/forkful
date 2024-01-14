---
title:                "Swift: Заголовок статті про програмування: Капіталізація рядка."
simple_title:         "Заголовок статті про програмування: Капіталізація рядка."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

В цій статті ми детально розглянемо як капіталізувати рядок в Swift. Капіталізація строку корисна для виведення тексту з правильними великими і малими літерами, що може бути корисним при написанні ігор або додатків з інтерфейсом користувача.

## Якість

Для капіталізації строки в Swift використовується метод .capitalized. Ось приклад використання:

```Swift
let text = "привіт, як вас звуть?"
let capitalizedText = text.capitalized
print(capitalizedText) // Виведе "Привіт, Як Вас Звуть?"
```

У вихідному рядку всі слова будуть починатися з великої літери. Також можна використовувати цей метод для перевірки, чи текст правильно введений користувачем, порівнювати рядки та більше.

```Swift
let name = "івано"
let correctName = "Івано"

if name.capitalized == correctName {
    print("Ім'я введено правильно.")
} else {
    print("Ім'я введено неправильно.")
}
// Виведе "Ім'я введено правильно."
```

## Детальний Аналіз

Крім методу .capitalized, також можна використовувати .uppercased та .lowercased для перетворення всіх символів в рядку на великі або малі літери відповідно. Також є можливість використовувати методи .localizedCapitalized та .localizedUppercased для капіталізації та перетворення великих літер у відповідності до мови телефону користувача.

## Дивитися Також

- [Документація Apple про капіталізацію строку](https://developer.apple.com/documentation/foundation/nsstring/1418184-capitalized)
- [Відео про роботу зі строками в Swift](https://www.youtube.com/watch?v=QTSxZmgEyzI)
- [Стаття про основи Swift для початківців](https://medium.com/swl-blog/swift-%D0%B4%D0%BB%D1%8F-%D0%BF%D0%BE%D1%87%D0%B0%D1%82%D0%BA%D1%96%D0%B2%D1%86%D1%96%D0%B2-%D1%83%D1%80%D0%BE%D0%BA-1-%D1%82%D0%B8-%D0%B6%D0%B5-%D0%BD%D0%B5%D0%BF%D1%80%D0%B0%D0%B2%D0%B8%D0%BB%D1%8C%D0%BD%D0%BE-%D0%BD%D0%B0%D0%B1%D0%B8%D1%80%D0%B0%D0%B5%D1%88-text-uilabel-bb4edf4a9408)