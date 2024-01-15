---
title:                "Розбір html"
html_title:           "Swift: Розбір html"
simple_title:         "Розбір html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

HTML - це одна з основних мов, яка використовується для створення веб-сторінок. Парсинг HTML може бути корисним для отримання текстової інформації з веб-сторінок для подальшого використання в скриптах або додатках.

## Як це зробити

Для початку, вам потрібно завантажити та імпортувати бібліотеку SwiftSoup, яка дозволяє парсити HTML. Далі, використовуйте функцію "do try catch" для зчитування HTML з веб-сторінки та перетворення його в об'єкт типу Document використовуючи бібліотеку SwiftSoup.

```swift
do {

    let html = try String(contentsOf: URL(string: "https://example.com")!)
    let doc: Document = try SwiftSoup.parse(html)

} catch Exception.Error(let type, let message) {

    print(message)
    
} catch {

    print("error")
}

```

Після цього, ви можете використовувати різні методи та функції бібліотеки SwiftSoup для отримання потрібної вам інформації з HTML сторінки. Наприклад, для отримання всього тексту з веб-сторінки, використовуйте наступний код:

```swift
do {

    let html = try String(contentsOf: URL(string: "https://example.com")!)
    let doc: Document = try SwiftSoup.parse(html)
    let text = try doc.text()

    print(text)

} catch Exception.Error(let type, let message) {

    print(message)
    
} catch {

    print("error")
}

```

## Детальний аналіз

Щоб розібратися з парсингом HTML детальніше, можна поглянути на різні методи та функції бібліотеки SwiftSoup, такі як "getElementsByClass", "getElementsById" або "select", які дозволяють вибирати елементи з HTML сторінки за допомогою CSS селекторів. Крім того, можна використовувати регулярні вирази для пошуку конкретної інформації на сторінці.

## Дивіться також

- [SwiftSoup бібліотека](https://github.com/scinfu/SwiftSoup)
- [Основи парсингу HTML в Swift](https://www.raywenderlich.com/2201-swift-algorithm-club-swift-html-parser-stack-queue#toc-anchor-002)
- [Офіційна документація SwiftSoup](https://jsoup.org/cookbook/extracting-data/selector-syntax)