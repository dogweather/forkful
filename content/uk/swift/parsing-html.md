---
title:                "Парсинг HTML"
date:                  2024-01-20T15:34:17.519705-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Що це таке & чому?
Розбір HTML – це процес аналізу HTML-коду для отримання даних. Програмісти роблять це, щоб зчитувати інформацію з веб-сторінок або модифікувати її.

## Як це зробити:
Використовуємо Swift і фреймворк SwiftSoup, щоб парсити HTML. Ось як:

```Swift
import SwiftSoup

func parseHTML(html: String) {
    do {
        let doc = try SwiftSoup.parse(html)
        let links = try doc.select("a")
        
        for link in links {
            let linkHref = try link.attr("href")
            let linkText = try link.text()
            print("\(linkText): \(linkHref)")
        }
    } catch {
        print("Error parsing HTML: \(error)")
    }
}

let html = "<html><head><title>Test</title></head><body><a href='http://example.com'>Example</a></body></html>"
parseHTML(html: html)
```

Вивід:
```
Example: http://example.com
```

## Глибоке занурення:
Розбір HTML не новий. З появою інтернету програмісти шукали способи читати веб-сторінки автоматично. Раніше використовувалися прості методи, такі як регулярні вирази, але це малоефективно для складного HTML.

Альтернативи? XPath, різні API для роботи з DOM. Кожен з цих інструментів має своє призначення, але SwiftSoup надає простий інтерфейс, який добре вписується у Swift екосистему.

Деталі? SwiftSoup імітує jQuery для спрощення синтаксису. Працюючи з ним, ви обираєте теги, класи чи ID, щоб знайти елементи сторінки. Розбір відбувається за допомогою внутрішньої структури документу, тож це безпечніше, ніж регулярні вирази, які можуть "зламатися" через складний HTML.

## Дивись також:
- Офіційна документація SwiftSoup: https://swifthsoup.lexfor.net/
- XPath: https://www.w3schools.com/xml/xpath_intro.asp
- Робота з DOM в Swift: https://developer.apple.com/documentation/webkit/dom_webkit_dom_classes