---
title:                "Використання регулярних виразів"
aliases: - /uk/swift/using-regular-expressions.md
date:                  2024-02-03T19:19:03.082798-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Регулярні вирази, або regex, це послідовності символів, які утворюють шаблон пошуку, часто використовувані для завдань зі збігами або маніпуляціями з рядками. Програмісти використовують їх для всього, від перевірки даних до парсингу та трансформацій, що робить їх незамінним інструментом в завданнях обробки та маніпуляції текстом у різноманітних мовах програмування, включно з Swift.

## Як:
Рідна підтримка regex в Swift використовує клас `NSRegularExpression`, разом з методами діапазону та заміни класу String. Нижче наведено приклад використання regex для пошуку та виділення електронних адрес у блоку тексту:

```swift
import Foundation

let text = "Зв'яжіться з нами за адресою support@example.com або feedback@example.org для отримання додаткової інформації."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Знайдено: \(text[range])")
        }
    } else {
        print("Збігів не знайдено.")
    }
} catch {
    print("Помилка regex: \(error.localizedDescription)")
}

// Приклад виводу:
// Знайдено: support@example.com
// Знайдено: feedback@example.org
```

Для більш складних або націлених на зручність сценаріїв, ви можете використовувати сторонні бібліотеки, такі як SwiftRegex, які спрощують синтаксис та розширюють можливості. Хоча стандартна бібліотека Swift могутня, деякі розробники віддають перевагу цим бібліотекам за їхній короткий синтаксис та додаткові можливості. Ось як ви можете виконати подібне завдання, використовуючи гіпотетичну сторонню бібліотеку:

```swift
// Припустимо, що існує бібліотека під назвою SwiftRegex і вона імпортована
let text = "Напишіть нам на hello@world.com або відвідайте наш сайт."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // Гіпотетичний метод, який надається SwiftRegex
if emails.isEmpty {
    print("Адрес електронної пошти не знайдено.")
} else {
    emails.forEach { email in
        print("Знайдено: \(email)")
    }
}

// Гіпотетичний вивід, припускаючи, що метод `matches(for:)` існує у SwiftRegex:
// Знайдено: hello@world.com
```

Цей приклад ілюструє використання стороннього пакету регулярних виразів для спрощення пошуку відповідностей у рядку, припускаючи, що існують такі зручні методи, як `matches(for:)`. Важливо звернутися до відповідної документації сторонньої бібліотеки для точного синтаксису та наявності методів.
