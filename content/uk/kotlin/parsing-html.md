---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Парсинг HTML - це процес вилучення специфічної інформації з HTML файлів. Програмісти роблять це для автоматизації перегляду веб-сторінок, сбору та обробки даних.

## Як це зробити:

Kotlin має відмінну бібліотеку для парсингу HTML - jsoup. Щоб здійснити парсинг, ось код:
```Kotlin
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element

fun main() {
    val htmlContent = "<html><head><title>Test</title></head><body><p>Hello, world!</p></body></html>"
    val document: Document = Jsoup.parse(htmlContent)
    val bodyElement: Element = document.body()
    println(bodyElement.text())  // Виведе: "Hello, world!"
}
```
Цей код перетворює HTML-рядок у `Document` об'єкт, а потім вилучає з нього текст з тіла сторінки.

## Глибше занурення

1. Історичний контекст: Парсинг HTML у незв'язних об'єктах був першим методом автоматизації перегляду веб-сторінок. Jsoup був створений, щоб програмісти могли легко працювати з HTML на високому рівні.
2. Альтернативи: Інші альтернативи для парсингу HTML в Kotlin включают HtmlCleaner та TagSoup.
3. Деталі реалізації: Jsoup парсить HTML, використовуючи DOM (Document Object Model), створюючи структуру дерева, яка дозволяє легко пересуватись та вилучати дані.

## Дивіться також

1. [Kotlin документація](https://kotlinlang.org/docs/home.html)
2. [Jsoup документація](https://jsoup.org/)
3. [HtmlCleaner документація](https://htmlcleaner.sourceforge.io/)