---
title:                "Парсинг HTML"
date:                  2024-01-20T15:33:05.522378-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? 
## Що та Чому?

Parsing HTML – це процес аналізу HTML-коду та перетворення його в структуровані дані. Програмісти роблять це, щоб витягти інформацію з веб-сторінок та працювати з нею як зі звичайними даними.

## How to:
## Як це зробити:

Kotlin, завдяки своїй бібліотеці Jsoup, робить парсинг HTML простим. Нижче наведено приклад коду та його виведення.

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>First parse</title></head>" +
               "<body><p>Parsed HTML into a doc.</p></body></html>"
    val doc = Jsoup.parse(html)
    val title = doc.title()
    val p = doc.select("p").first().ownText()

    println("Title: $title")
    println("First paragraph: $p")
}

// Output:
// Title: First parse
// First paragraph: Parsed HTML into a doc.
```

## Deep Dive:
## Поглиблено:

Jsoup – це зручна бібліотека Java для роботи з HTML, і вона без проблем використовується у Kotlin. Історично, парсинг HTML був складнішим через неконсистентність HTML-коду на різних сайтах. Jsoup допомагає обробляти неправильно сформульований HTML, створюючи DOM-структуру, яка легко маніпулюється.

Альтернативою Jsoup є інші бібліотеки, наприклад HtmlUnit чи TagSoup. Проте Jsoup вважається однією з найзручніших для Kotlin/Java через свою виразну "fluent" API та здатність обробляти реальні сценарії веб-скрапінгу.

Деталі реалізації Jsoup включають можливість чуттєво вибирати елементи за допомогою CSS-селекторів, поводження з формами, а також багато чого іншого, що спрощує роботу з даними HTML.

## See Also:
## Додатково:

- [Jsoup Official Website](https://jsoup.org/)
- [Jsoup GitHub Repository](https://github.com/jhy/jsoup/)
- [Kotlin Programming Language Website](https://kotlinlang.org/)
