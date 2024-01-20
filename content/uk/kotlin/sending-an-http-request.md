---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що та Навіщо?

Відправлення HTTP-запиту - це процес, за допомогою якого програма встановлює зв'язок з веб-сервером, щоб отримати або відправити дані. Програмісти роблять це, щоб взаємодіяти з веб-ресурсами, наприклад, для отримання даних з API.

## Як це робити:

```Kotlin
import java.net.URL
import java.net.HttpURLConnection

fun main() {
    val connection = URL("http://example.com").openConnection() as HttpURLConnection
    connection.requestMethod = "GET"
    
    val response = connection.inputStream.bufferedReader().use { it.readText() }  
    println(response) // виводимо відповідь сервера
}
```
У цьому коді ми відкриваємо з'єднання з URL "http://example.com", встановлюємо метод запиту як "GET", а потім читаємо та виводимо відповідь сервера.

## Поглиблений погляд:

Відправлення HTTP-запитів було ключовою частиною веб-програмування з моменту створення HTTP у 1991 році. У Kotlin це ще простіше за допомогою вбудованих бібліотек Java.

Ви також можете використовувати альтернативи, як-то OkHttp чи Ktor, для більш складних задач, таких як асинхронні запити або автоматичне обробка JSON. Однак у більшості випадків базові засоби Java будуть достатніми.

Використовуючи HttpURLConnection, ви вручну керуєте всіма аспектами відправки запитів, такими як встановлення заголовків, управління відповідями тощо.

## Див. також:

- OkHttp: https://square.github.io/okhttp
- Ktor: https://ktor.io/clients/http-client.html
- Більше інформації про HTTP-запити: https://developer.mozilla.org/uk/docs/Web/HTTP/Overview
- HttpURLConnection: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html