---
title:                "Аналіз HTML"
aliases:
- uk/kotlin/parsing-html.md
date:                  2024-02-03T19:12:47.468649-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Розбір HTML означає декомпозицію розмітки веб-сторінки на щось, що програма може розуміти та маніпулювати. Програмісти аналізують HTML, щоб витягувати дані, автоматизувати веб-взаємодії або мігрувати контент між системами.

## Як це зробити:
Kotlin робить розбір HTML простим завдяки бібліотекам на кшталт Jsoup. Ось як це робиться:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Зразок сторінки</title></head><body><p>Це тест.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Назва: $title")  // Вивід: Назва: Зразок сторінки

    val pText = doc.select("p").first()?.text()
    println("Абзац: $pText")  // Вивід: Абзац: Це тест.
}
```

Ми витягуємо назву та текст абзацу, що є лише вершиною айсберга того, що може зробити Jsoup. Але це хороший початок.

## Поглиблений аналіз:
До Kotlin, Java була основним інструментом для цього, часто незграбно. Jsoup змінив правила гри, надаючи підхід на кшталт jQuery. Проте розбір HTML не обмежується лише Jsoup; існують й інші бібліотеки на кшталт HtmlUnit або навіть регулярні вирази (хоча це не рекомендується). З Jsoup ви забезпечуєте, що ваш розбір поважає структуру документа. Вона використовує модель DOM, що дозволяє вибирати та маніпулювати елементами. Вона також стійка - може розбирати навіть найбільш заплутаний HTML.

## Дивіться також:
Зануртесь глибше у Jsoup:

- Офіційна документація Jsoup: https://jsoup.org/
- Книга "Kotlin для розробників Android": https://antonioleiva.com/kotlin-android-developers-book/
- Офіційний сайт мови програмування Kotlin: https://kotlinlang.org/

Для більш широких обговорень та туторіалів на тему веб-скрапінгу та розбору:

- Веб-скрапінг з Kotlin та Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Розбір HTML на Android з Kotlin та Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
