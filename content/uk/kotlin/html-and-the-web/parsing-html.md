---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:47.468649-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Kotlin \u0440\u043E\u0431\u0438\u0442\u044C \u0440\u043E\u0437\u0431\u0456\u0440\
  \ HTML \u043F\u0440\u043E\u0441\u0442\u0438\u043C \u0437\u0430\u0432\u0434\u044F\
  \u043A\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430\u043C\
  \ \u043D\u0430 \u043A\u0448\u0442\u0430\u043B\u0442 Jsoup. \u041E\u0441\u044C \u044F\
  \u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u044C\u0441\u044F."
lastmod: '2024-03-13T22:44:49.215761-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u0440\u043E\u0431\u0438\u0442\u044C \u0440\u043E\u0437\u0431\u0456\
  \u0440 HTML \u043F\u0440\u043E\u0441\u0442\u0438\u043C \u0437\u0430\u0432\u0434\u044F\
  \u043A\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430\u043C\
  \ \u043D\u0430 \u043A\u0448\u0442\u0430\u043B\u0442 Jsoup."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

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
