---
title:                "Kotlin: Розбір html"
simple_title:         "Розбір html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Чому
Парсинг HTML є надзвичайно корисним в контексті розробки веб-скраперів, моніторингу веб-сторінок та автоматизації процесів збору даних. Використовуючи Kotlin, ви можете швидко та ефективно отримувати потрібну вам інформацію з веб-сторінок.

## Як
```Kotlin
import org.jsoup.Jsoup
import java.io.File

fun main() {
    // створіть змінну для збереження HTML-коду
    val html = File("index.html").readText()
    // використовуйте бібліотеку Jsoup для парсингу HTML-коду
    val doc = Jsoup.parse(html)
    // виберіть всі елементи з тегом "a"
    val links = doc.select("a")
    // виведіть посилання з кожного елементу
    for (link in links) {
        println(link.attr("href"))
    }
}
```
Припустимо, вам потрібно отримати всі посилання на веб-сторінці. Завантажте HTML-код сторінки за допомогою функції `readText` з бібліотеки `kotlin.io`, використовуйте бібліотеку Jsoup для парсингу коду та відфільтруйте елементи за допомогою пошуку за тегом. Для кожного елементу можете отримати потрібні вам атрибути, такі як `href` для отримання посилань.

## Вглиб
Парсинг HTML-коду може бути складним завданням. Варто детальніше розібратися зі структурою HTML-документів та документацією бібліотеки Jsoup. Також варто звернути увагу на різні способи пошуку та фільтрації елементів, наприклад, за допомогою CSS-селекторів.

## Дивіться також
- [Kotlin офіційний сайт](https://kotlinlang.org/)
- [Бібліотека Jsoup](https://jsoup.org/)
- [Приклади парсингу HTML з використанням Kotlin](https://github.com/topics/parse-html-kotlin)