---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:03.082798-07:00
description: "\u042F\u043A: \u0420\u0456\u0434\u043D\u0430 \u043F\u0456\u0434\u0442\
  \u0440\u0438\u043C\u043A\u0430 regex \u0432 Swift \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0454 \u043A\u043B\u0430\u0441 `NSRegularExpression`,\
  \ \u0440\u0430\u0437\u043E\u043C \u0437 \u043C\u0435\u0442\u043E\u0434\u0430\u043C\
  \u0438 \u0434\u0456\u0430\u043F\u0430\u0437\u043E\u043D\u0443 \u0442\u0430 \u0437\
  \u0430\u043C\u0456\u043D\u0438 \u043A\u043B\u0430\u0441\u0443 String. \u041D\u0438\
  \u0436\u0447\u0435 \u043D\u0430\u0432\u0435\u0434\u0435\u043D\u043E \u043F\u0440\
  \u0438\u043A\u043B\u0430\u0434\u2026"
lastmod: '2024-03-13T22:44:49.902435-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0456\u0434\u043D\u0430 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\
  \u043A\u0430 regex \u0432 Swift \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0454 \u043A\u043B\u0430\u0441 `NSRegularExpression`, \u0440\u0430\
  \u0437\u043E\u043C \u0437 \u043C\u0435\u0442\u043E\u0434\u0430\u043C\u0438 \u0434\
  \u0456\u0430\u043F\u0430\u0437\u043E\u043D\u0443 \u0442\u0430 \u0437\u0430\u043C\
  \u0456\u043D\u0438 \u043A\u043B\u0430\u0441\u0443 String."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

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
