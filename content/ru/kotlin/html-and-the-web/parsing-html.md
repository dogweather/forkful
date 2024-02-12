---
title:                "Разбор HTML"
aliases: - /ru/kotlin/parsing-html.md
date:                  2024-01-28T23:59:53.197388-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Разбор HTML подразумевает анализ разметки веб-страницы на компоненты, которые программа может понять и обработать. Программисты анализируют HTML, чтобы извлекать данные, автоматизировать веб-взаимодействия или мигрировать содержимое между системами.

## Как это сделать:
Kotlin упрощает разбор HTML с помощью библиотек, таких как Jsoup. Вот как это делается:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Пример страницы</title></head><body><p>Это тест.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Заголовок: $title")  // Вывод: Заголовок: Пример страницы

    val pText = doc.select("p").first()?.text()
    println("Параграф: $pText")  // Вывод: Параграф: Это тест.
}
```

Мы извлекаем заголовок и текст параграфа, лишь затрагивая верхушку айсберга того, что может Jsoup. Но это хорошее начало.

## Погружение:
До Kotlin, Java был предпочтительным языком для этого, зачастую неуклюже. Jsoup изменил ситуацию, предлагая подход, похожий на jQuery. Однако разбор HTML не ограничивается только Jsoup; существуют и другие библиотеки, такие как HtmlUnit, или даже регулярные выражения (хотя это не рекомендуется). С Jsoup вы можете быть уверены, что ваш разбор учитывает структуру документа. Он использует модель DOM, что позволяет выбирать и манипулировать элементами. К тому же он устойчив - может разбирать даже самый запутанный HTML.

## См. также:
Подробнее о Jsoup:

- Официальная документация Jsoup: https://jsoup.org/
- Книга "Kotlin для разработчиков Android": https://antonioleiva.com/kotlin-android-developers-book/
- Официальный сайт языка программирования Kotlin: https://kotlinlang.org/

Для более широких дискуссий и уроков о веб-скрапинге и анализе:

- Веб-скрапинг с Kotlin и Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Разбор HTML на Android с Kotlin и Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
