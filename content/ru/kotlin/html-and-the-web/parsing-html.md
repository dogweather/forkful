---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:53.197388-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Kotlin \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u0440\u0430\
  \u0437\u0431\u043E\u0440 HTML \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A, \u0442\u0430\u043A\u0438\
  \u0445 \u043A\u0430\u043A Jsoup. \u0412\u043E\u0442 \u043A\u0430\u043A \u044D\u0442\
  \u043E \u0434\u0435\u043B\u0430\u0435\u0442\u0441\u044F."
lastmod: '2024-03-13T22:44:44.974264-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u0440\u0430\u0437\
  \u0431\u043E\u0440 HTML \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A, \u0442\u0430\u043A\u0438\u0445\
  \ \u043A\u0430\u043A Jsoup."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

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
