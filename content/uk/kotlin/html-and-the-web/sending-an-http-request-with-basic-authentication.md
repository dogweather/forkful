---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases:
- /uk/kotlin/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:01.106957-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і Чому?

Відправка HTTP-запиту з базовою аутентифікацією – це метод захисту доступу до ресурсів вебсервісу через логін і пароль. Програмісти використовують це для забезпечення безпеки та обмеженого доступу до API або веб-ресурсів.

## Як це зробити:

```Kotlin
import java.net.URL
import java.util.Base64
import javax.net.ssl.HttpsURLConnection

fun sendBasicAuthRequest(username: String, password: String, endpoint: String) {
    val url = URL(endpoint)
    val connection = url.openConnection() as HttpsURLConnection

    val credentials = "$username:$password"
    val encodedCredentials = Base64.getEncoder().encodeToString(credentials.toByteArray(Charsets.UTF_8))
    connection.setRequestProperty("Authorization", "Basic $encodedCredentials")

    connection.apply {
        requestMethod = "GET"
        doInput = true
        doOutput = true
    }

    println("Response Code: ${connection.responseCode}")
    println("Response Message: ${connection.responseMessage}")
}

fun main() {
    val username = "user"
    val password = "pass"
    val endpoint = "https://yourapi.com/data"

    sendBasicAuthRequest(username, password, endpoint)
}
```

Виходи:

```
Response Code: 200
Response Message: OK
```

## Поглиблено:

Базова аутентифікація з'явилася ще на світанку вебу і досі користується популярністю через свою простоту. Хоча зараз існує більш безпечні методи, наприклад OAuth 2.0, базова аутентифікація залишається значущою для швидких або внутрішніх рішень. Значення `"Authorization"` header кодується у Base64 і включає `username` та `password`, розділені двокрапкою. Варто пам'ятати, що Base64 не є шифруванням і його легко декодувати; тому використання HTTPS є важливим для забезпечення безпеки. У Kotlin для HTTP-запитів з автентифікацією можна користуватися різними бібліотеками, але базовий приклад використовує інтегровані Java класи.

## Дивіться також:

- [Base64 Encoding in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [Understanding Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Java™ Secure Socket Extension (JSSE) Reference Guide](https://docs.oracle.com/javase/8/docs/technotes/guides/security/jsse/JSSERefGuide.html)
