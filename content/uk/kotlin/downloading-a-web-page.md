---
title:                "Завантаження веб-сторінки"
html_title:           "Kotlin: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Завантаження веб-сторінки може бути корисним для збереження контенту з Інтернету для подальшого використання, а також для доступу до інформації в офлайн режимі.

## Як

Цей приклад використовує бібліотеку `kotlinx.coroutines` для створення синхронного запиту на отримання вмісту веб-сторінки.

За допомогою `runBlocking` функції, ми можемо виконати запит і очікувати результату перед продовженням програми.

```Kotlin
import kotlinx.coroutines.*
import java.net.URL

fun main() { 
    runBlocking {
        val url = URL("https://example.com") // вказуємо URL сторінки
        val content = url.readText() // отримуємо вміст сторінки як рядок
        println(content) // виводимо вміст у консоль
    }
}
```

Результатом виконання програми буде виведений у консоль вміст веб-сторінки.

## Глибокий занурений погляд

Завантаження веб-сторінки за допомогою `readText()` може бути зручною, але менш ефективною методологією, оскільки це виконує синхронний запит, тобто програма буде зупинена на час отримання відповіді від сервера.

У більш складних ситуаціях, коли потрібен асинхронний запит або більш розширена обробка вмісту сторінки, рекомендовано використовувати бібліотеку `khttp`, яка надає більш гнучкий інструментарій для роботи з HTTP запитами.

## Дивись також

- [Офіційна документація з бібліотекою kotlinx.coroutines](https://kotlinlang.org/docs/reference/coroutines-overview.html)
- [Офіційна документація з бібліотекою khttp](https://khttp.readthedocs.io/en/latest/)
- [Стаття про асинхронні HTTP запити в Kotlin](https://www.baeldung.com/kotlin-http-requests)