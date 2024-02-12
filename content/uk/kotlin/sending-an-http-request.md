---
title:                "Надсилання HTTP-запиту"
aliases:
- uk/kotlin/sending-an-http-request.md
date:                  2024-01-20T18:00:24.897375-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Надсилання HTTP запиту - це спосіб спілкування вашої програми з веб-сервісами, щоб отримати або відправити дані. Програмісти роблять це для інтеграції з API, а також для взаємодії з вебом.

## Як це зробити:
Запит HTTP у Kotlin можна виконати декількома способами. Один із популярних - використання бібліотеки ktor. Давайте спробуємо:

```Kotlin
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*
import io.ktor.client.statement.*

suspend fun main() {
    val client = HttpClient(CIO)
    
    try {
        val response: HttpResponse = client.get("https://ktor.io/")
        println(response.status)
        println(response.readText())
    } finally {
        client.close()
    }
}
```

В результаті вас вітатиме статус відповіді та HTML вміст сторінки.

## Поглиблений Розгляд:
Написання HTTP запитів стало загальним завданням у програмуванні про повсякчасному зростанні мережевих сервісів та API. У давніші часи, це було більш складно та вимагало більше коду. Тепер, з бібліотеками як ktor або OkHttpClient, завдання стало набагато легшим.

Альтернативи ktor - це HttpClient з Java, Retrofit чи Apache HttpComponents. Кожна має свої особливості: Retrofit оптимізований під REST API, а HttpClient з Java - це більш низькорівневий інструмент.

ktor використовує корутини Kotlin для асинхронних викликів, роблячи код чистим і легким для читання. Більше того, ktor дозволяє налаштовувати запити, додавати проміжне програмне забезпечення та обробляти відповіді з допомогою DSL.

## Дивись Також:
- [OkHttp](https://square.github.io/okhttp/)
- [Apache HttpComponents](https://hc.apache.org/)
- [Retrofit](https://square.github.io/retrofit/)
