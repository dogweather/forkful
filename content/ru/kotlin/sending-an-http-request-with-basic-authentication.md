---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
date:                  2024-01-29T00:02:55.369674-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Базовая аутентификация добавляет комбинацию имя пользователя:пароль к HTTP-запросу. Разработчики используют ее как быстрый и грязный способ доказать, кто и что запрашивает в интернете.

## Как это сделать:

Kotlin обрабатывает HTTP-запросы с помощью библиотек, таких как `ktor` или `okhttp`. Давайте на данный момент остановимся на `okhttp`.

Сначала добавьте библиотеку в ваш build.gradle:

```groovy
dependencies {
    implementation("com.squareup.okhttp3:okhttp:4.9.0")
}
```

Пора кодить:

```kotlin
import okhttp3.Credentials
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.IOException

fun main() {
    val client = OkHttpClient()

    val username = "admin"
    val password = "password123"
    val credentials = Credentials.basic(username, password)

    val request = Request.Builder()
        .url("http://example.com/resource")
        .header("Authorization", credentials)
        .build()

    client.newCall(request).execute().use { response ->
        if (!response.isSuccessful) throw IOException("Unexpected code $response")

        println(response.body!!.string())
    }
}
```

Нажмите выполнить и наблюдайте за вашей консолью. Вы должны увидеть вывод защищенного ресурса.

## Глубже

В прошлом базовая HTTP-аутентификация была основным методом. Просто: закодируйте в base64 `имя пользователя:пароль` и поместите это в заголовок. Не безопасно в одиночку, поэтому на вечеринку пришел HTTPS.

Альтернативы? Множество. OAuth для токенов, API ключи для простоты, или digest-аутентификация для обновления. Базовая аутентификация хороша для старта или для внутренних инструментов, но не для современного, ориентированного на безопасность веба.

Деталь реализации: Не изобретайте колесо. Библиотеки обрабатывают кодирование и нюансы протокола. OkHttp даже занимается повторными попытками и соединениями за вас. Помните, базовая аутентификация через HTTP - это табу, всегда используйте HTTPS, чтобы сохранить учетные данные в безопасности во время передачи.

## Смотрите также

- Официальная документация OkHttp: [https://square.github.io/okhttp/](https://square.github.io/okhttp/)
- Страница языка Kotlin (для всего, что касается Kotlin): [https://kotlinlang.org/](https://kotlinlang.org/)
- Узнать больше о базовой аутентификации: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Альтернативы базовой аутентификации, такие как OAuth 2.0: [https://oauth.net/2/](https://oauth.net/2/)
