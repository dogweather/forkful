---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:56.180248-07:00
description: "\u042F\u043A: Swift \u0437\u0430 \u0437\u0430\u043C\u043E\u0432\u0447\
  \u0443\u0432\u0430\u043D\u043D\u044F\u043C \u043D\u0435 \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0443 \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443 \u0434\u043B\u044F \u0440\
  \u043E\u0437\u0431\u043E\u0440\u0443 HTML, \u0449\u043E \u043F\u043E\u0442\u0440\
  \u0435\u0431\u0443\u0454 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\
  \u043D\u044F \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\u0445 \u0431\u0456\
  \u0431\u043B\u0456\u043E\u0442\u0435\u043A \u0434\u043B\u044F \u0435\u0444\u0435\
  \u043A\u0442\u0438\u0432\u043D\u043E\u0433\u043E \u0432\u0438\u043A\u043E\u043D\u0430\
  \u043D\u043D\u044F \u0446\u044C\u043E\u0433\u043E\u2026"
lastmod: '2024-03-13T22:44:49.915599-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0437\u0430 \u0437\u0430\u043C\u043E\u0432\u0447\u0443\u0432\u0430\
  \u043D\u043D\u044F\u043C \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\u0430\u0454\
  \ \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0443 \u0431\u0456\u0431\u043B\
  \u0456\u043E\u0442\u0435\u043A\u0443 \u0434\u043B\u044F \u0440\u043E\u0437\u0431\
  \u043E\u0440\u0443 HTML, \u0449\u043E \u043F\u043E\u0442\u0440\u0435\u0431\u0443\
  \u0454 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F\
  \ \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\u0445 \u0431\u0456\u0431\u043B\
  \u0456\u043E\u0442\u0435\u043A \u0434\u043B\u044F \u0435\u0444\u0435\u043A\u0442\
  \u0438\u0432\u043D\u043E\u0433\u043E \u0432\u0438\u043A\u043E\u043D\u0430\u043D\u043D\
  \u044F \u0446\u044C\u043E\u0433\u043E \u0437\u0430\u0432\u0434\u0430\u043D\u043D\
  \u044F."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

## Як:
Swift за замовчуванням не включає вбудовану бібліотеку для розбору HTML, що потребує використання сторонніх бібліотек для ефективного виконання цього завдання. Одним з найпопулярніших варіантів є SwiftSoup, чиста бібліотека Swift, яка пропонує синтаксис подібний до jQuery для розбору і маніпуляції HTML.

### Встановлення
Спочатку вам потрібно додати SwiftSoup до вашого проєкту. Якщо ви використовуєте Swift Package Manager, можете додати її до залежностей у вашому `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.3.2")
]
```

### Приклад: Витягування посилань з HTML
Припустимо, у вас є HTML-документ, і ви хочете витягнути всі посилання (`<a href="...">`). З SwiftSoup ви можете зробити це легко:

```swift
import SwiftSoup

let html = """
<!DOCTYPE html>
<html>
<head>
    <title>Зразкова сторінка</title>
</head>
<body>
    <p>Ласкаво просимо на наш сайт</p>
    <a href="https://example.com/page1">Сторінка 1</a>
    <a href="https://example.com/page2">Сторінка 2</a>
</body>
</html>
"""

do {
    let doc: Document = try SwiftSoup.parse(html)
    let links: Elements = try doc.select("a")
    for link in links.array() {
        let linkHref: String = try link.attr("href")
        let linkText: String = try link.text()
        print("\(linkText) - \(linkHref)")
    }
} catch Exception.Error(let type, let message) {
    print("Тип помилки: \(type) Повідомлення: \(message)")
} catch {
    print("помилка")
}
```

### Приклад виводу
Попередній код витягує URL-адреси та їх текст з HTML, виводячи:

```
Сторінка 1 - https://example.com/page1
Сторінка 2 - https://example.com/page2
```

Цей базовий приклад демонструє, як використовувати SwiftSoup для розбору HTML-документів. Досліджуючи документацію SwiftSoup далі, ви можете знайти численні методи для навігації, пошуку та модифікації HTML-вмісту, надаючи вашим додаткам Swift змогу легко обробляти складний веб-вміст.
