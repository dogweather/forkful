---
title:                "Аналіз HTML"
aliases:
- /uk/swift/parsing-html.md
date:                  2024-02-03T19:13:56.180248-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?
Розбір HTML означає процес розбиття та інтерпретації структури HTML-вмісту, зазвичай для вилучення певних даних або програмної маніпуляції з цим вмістом. Програмісти займаються розбором HTML для веб-скрапінгу, збору даних, автоматизованого тестування та міграції вмісту, дозволяючи додаткам ефективно взаємодіяти з веб-документами та обробляти їх.

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
