---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:55.369674-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Kotlin \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\
  \u0435\u0442 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u044B \u0441 \u043F\u043E\
  \u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\
  , \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A `ktor` \u0438\u043B\u0438 `okhttp`.\
  \ \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043D\u0430 \u0434\u0430\u043D\u043D\
  \u044B\u0439 \u043C\u043E\u043C\u0435\u043D\u0442 \u043E\u0441\u0442\u0430\u043D\
  \u043E\u0432\u0438\u043C\u0441\u044F \u043D\u0430 `okhttp`.\u2026"
lastmod: '2024-03-13T22:44:44.977858-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\u0435\
  \u0442 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u044B \u0441 \u043F\u043E\u043C\
  \u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\
  , \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A `ktor` \u0438\u043B\u0438 `okhttp`."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

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
