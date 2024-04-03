---
date: 2024-01-20 18:02:01.106957-07:00
description: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2013 \u0446\u0435 \u043C\u0435\u0442\u043E\u0434 \u0437\u0430\u0445\u0438\u0441\
  \u0442\u0443 \u0434\u043E\u0441\u0442\u0443\u043F\u0443 \u0434\u043E \u0440\u0435\
  \u0441\u0443\u0440\u0441\u0456\u0432 \u0432\u0435\u0431\u0441\u0435\u0440\u0432\u0456\
  \u0441\u0443 \u0447\u0435\u0440\u0435\u0437 \u043B\u043E\u0433\u0456\u043D \u0456\
  \ \u043F\u0430\u0440\u043E\u043B\u044C. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.218385-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2013 \u0446\u0435 \u043C\u0435\u0442\u043E\u0434 \u0437\u0430\u0445\u0438\u0441\
  \u0442\u0443 \u0434\u043E\u0441\u0442\u0443\u043F\u0443 \u0434\u043E \u0440\u0435\
  \u0441\u0443\u0440\u0441\u0456\u0432 \u0432\u0435\u0431\u0441\u0435\u0440\u0432\u0456\
  \u0441\u0443 \u0447\u0435\u0440\u0435\u0437 \u043B\u043E\u0433\u0456\u043D \u0456\
  \ \u043F\u0430\u0440\u043E\u043B\u044C."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

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
