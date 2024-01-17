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

Що і навіщо?

Надсилання HTTP-запиту з базовою аутентифікацією - це процес, при якому програміст надсилає запит до сервера, який обробляється і відповідає згідно з правами доступу користувача. Це важливо, коли потрібно отримати обмежений доступ до інформації або здійснити певну дію на сервері.

Як це зробити?

```Kotlin
val credentials = "username:password".toByteArray()
val encodedCredentials = Base64.encode(credentials)
val url = "https://example.com/api"
val request = Request.Builder()
    .url(url)
    .header("Authorization", "Basic $encodedCredentials")
    .build()
val client = OkHttpClient()
val response = client.newCall(request).execute()
println(response.body()?.string())
```

Глибоке занурення

Базова аутентифікація була вперше введена в HTTP протокол у 1995 році та представлена в RFC 1945. Це базовий метод аутентифікації, який працює на основі кодування ім'я користувача та пароля в форматі Base64. Є інші методи аутентифікації, такі як OAuth, які забезпечують більш безпечну і автоматичну аутентифікацію.

Також, варто зазначити, що використання базової аутентифікації не є надійним засобом захисту, оскільки ім'я користувача та пароль передаються у відкритому вигляді, тому рекомендується використовувати HTTPS для забезпечення безпеки передачі даних.

Дивіться також

- https://tools.ietf.org/html/rfc1945 - RFC 1945 про базову аутентифікацію
- https://oauth.net/2/ - офіційна сторінка OAuth