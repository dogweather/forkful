---
date: 2024-01-20 18:02:03.881772-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \uFF0C\u662F\u901A\u8FC7\u7F51\u7EDC\u4F20\u8F93\u7528\u6237\u540D\u548C\u5BC6\u7801\
  \u6765\u8BA4\u8BC1\u7528\u6237\u7684\u65B9\u5F0F\u3002\u5F00\u53D1\u8005\u7528\u5B83\
  \u6765\u4FDD\u8BC1\u6570\u636E\u4EA4\u6362\u7684\u5B89\u5168\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.718950-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\uFF0C\
  \u662F\u901A\u8FC7\u7F51\u7EDC\u4F20\u8F93\u7528\u6237\u540D\u548C\u5BC6\u7801\u6765\
  \u8BA4\u8BC1\u7528\u6237\u7684\u65B9\u5F0F\u3002\u5F00\u53D1\u8005\u7528\u5B83\u6765\
  \u4FDD\u8BC1\u6570\u636E\u4EA4\u6362\u7684\u5B89\u5168\u3002."
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
