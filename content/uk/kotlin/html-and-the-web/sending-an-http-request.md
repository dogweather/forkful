---
date: 2024-01-20 18:00:24.897375-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0417\u0430\u043F\u0438\u0442 HTTP \u0443 Kotlin \u043C\u043E\u0436\u043D\u0430\
  \ \u0432\u0438\u043A\u043E\u043D\u0430\u0442\u0438 \u0434\u0435\u043A\u0456\u043B\
  \u044C\u043A\u043E\u043C\u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0430\u043C\u0438\
  . \u041E\u0434\u0438\u043D \u0456\u0437 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\
  \u043D\u0438\u0445 - \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\
  \u043D\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438 ktor.\
  \ \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0441\u043F\u0440\u043E\u0431\u0443\
  \u0454\u043C\u043E."
lastmod: '2024-03-13T22:44:49.214118-06:00'
model: gpt-4-1106-preview
summary: "\u0417\u0430\u043F\u0438\u0442 HTTP \u0443 Kotlin \u043C\u043E\u0436\u043D\
  \u0430 \u0432\u0438\u043A\u043E\u043D\u0430\u0442\u0438 \u0434\u0435\u043A\u0456\
  \u043B\u044C\u043A\u043E\u043C\u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0430\u043C\
  \u0438."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

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
