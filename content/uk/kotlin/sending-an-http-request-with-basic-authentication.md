---
title:                "Надсилання запиту http з базовою аутентифікацією"
html_title:           "Kotlin: Надсилання запиту http з базовою аутентифікацією"
simple_title:         "Надсилання запиту http з базовою аутентифікацією"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Базова автентифікація - це один з найпростіших методів захисту данних при відправці HTTP запитів. Вона дозволяє передавати ідентифікатор користувача та пароль разом з запитом, щоб сервер міг перевірити права доступу і забезпечити безпечне з'єднання.

## Як це зробити

Для відправлення HTTP запиту з базовою автентифікацією використовується `URL` та `HttpURLConnection` класи. Спочатку ми створюємо об'єкт URL з адресою, до якої хочемо зробити запит:

```Kotlin
val url = URL("https://example.com/api/endpoint")
```

Далі потрібно створити об'єкт `HttpURLConnection` і встановити метод запиту (GET, POST, PUT тощо):

```Kotlin
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
```

Тепер можемо встановити базову автентифікацію, передавши ідентифікатор користувача та пароль у вигляді строки кодуваного Base64:

```Kotlin
val username = "user"
val password = "password"
val auth = "$username:$password".toByteArray().encodeBase64()
connection.setRequestProperty("Authorization", "Basic $auth")
```

На останньому кроці потрібно передати запит, отримати відповідь та обробити її результат:

```Kotlin
val response = connection.inputStream.bufferedReader().use {
    it.readText()
}
println(response) // виводимо отриману відповідь
```

## Глибший аналіз

Базова автентифікація - це дуже простий метод захисту, який не забезпечує надійного захисту. Якщо для вас важлива безпека данних, рекомендовано використовувати більш сучасні методи аутентифікації, такі як OAuth.

## Дивись також

- [Офіційна документація Kotlin](https://kotlinlang.org/docs/reference/)
- [Стаття про аутентифікацію в Android за допомогою Retrofit та OkHttp](https://proandroiddev.com/authentication-in-android-using-retrofit-and-okhttp-a2794a415426)