---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:02:03.881772-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
发送带有基本认证的HTTP请求，是通过网络传输用户名和密码来认证用户的方式。开发者用它来保证数据交换的安全。

## How to:
Kotlin中使用HttpURLConnection和Base64编码来实现带基本认证的HTTP请求。

```kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun sendGetRequestWithBasicAuth(url: String, username: String, password: String) {
    val connection = URL(url).openConnection() as HttpURLConnection
    val auth = Base64.getEncoder().encodeToString("$username:$password".toByteArray())
    
    connection.apply {
        requestMethod = "GET"
        setRequestProperty("Authorization", "Basic $auth")
        inputStream.bufferedReader().use { reader ->
            println(reader.readText())
        }
    }
}

fun main() {
    val url = "https://api.example.com/data"
    val username = "user"
    val password = "pass"
    
    sendGetRequestWithBasicAuth(url, username, password)
}
// Sample Output:
// {"data": "some secure data"}
```

## Deep Dive
基本认证（Basic Authentication）是1990年代初就出现的一种HTTP认证方式。虽然有更安全的方法，如OAuth2，它依然在APIs中常被用于内部或低安全要求的场合。这种认证方式将用户名和密码用冒号连接后进行Base64编码，附加在请求头的Authorization字段中发送给服务器。虽简单，但Base64不是加密方式，所以通信应始终在HTTPS协议下进行，保证传输的安全性。

## See Also
- HTTP基本认证介绍: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Kotlin 编程语言官网: [Kotlin](https://kotlinlang.org/)
- 安全的HTTP认证方式介绍: [OWASP](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)