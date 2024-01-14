---
title:                "Kotlin: Надсилання http-запиту з базовою аутентифікацією"
simple_title:         "Надсилання http-запиту з базовою аутентифікацією"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP-запиту з базовою аутентифікацією - це важливе поняття для будь-якого розробника програмного забезпечення. Це дозволяє надійно передавати користувачам інформацію з захищених ресурсів.

## Як це зробити

Для того, щоб надіслати HTTP-запит з базовою аутентифікацією, вам знадобиться вказати URL, HTTP метод, а також ім'я користувача та пароль для аутентифікації. Для цього використовуйте бібліотеку `kotlinx`. Ось приклад коду:

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*
import io.ktor.client.authentication.*

suspend fun main() {
    // Вказуємо URL, метод та аутентифікацію
    val client = HttpClient {
        install(Authentication) {
            basic {
                username = "JohnDoe"
                password = "secret"
            }
        }
    }

    // Надсилаємо GET-запит до захищеного ресурсу
    val response: String = client.get("https://example.com/protected")
    println(response)
}

```
Вивід: `Доступ успішно наданий!`

## Глибоке копання

При надсиланні HTTP-запитів з базовою аутентифікацією, необхідно зазначити користувача та пароль у заголовках запиту. Для цього просто додайте наступну строку до бібліотеки `kotlinx`:

```Kotlin
header("Authorization", "${HttpAuthHeader.basic(username, password)}")
```

Також важливо пам'ятати, що ім'я користувача та пароль повинні бути закодовані у форматі base64.

## Дивись також

- [Офіційна документація по бібліотеці `kotlinx`](https://ktor.io/clients/http-client/quick-start/requests.html)
- [Стаття про базову аутентифікацію](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)