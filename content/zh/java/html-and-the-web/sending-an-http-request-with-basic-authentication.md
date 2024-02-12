---
title:                "使用基本认证发送 HTTP 请求"
aliases: - /zh/java/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:57.120126-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
发送带有基本认证的HTTP请求是一种通过网络向服务器验证身份的方式。程序员这么做主要是为了安全地访问受保护的资源。

## 如何：
```java
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;

public class BasicAuthExample {
    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com/api/data");
            Authenticator.setDefault(new Authenticator() {
                protected PasswordAuthentication getPasswordAuthentication() {
                    return new PasswordAuthentication("username", "password".toCharArray());
                }
            });

            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");

            int status = connection.getResponseCode();
            System.out.println("Response Code: " + status);

            BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String inputLine;
            StringBuilder content = new StringBuilder();

            while ((inputLine = in.readLine()) != null) {
                content.append(inputLine);
            }

            in.close();
            System.out.println("Response Body: " + content);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
输出样例：
```
Response Code: 200
Response Body: { "data": "This is the data from the server." }
```

## 深潜
发送带有基本认证的HTTP请求一直是一个老生常谈的话题。它的历史可追溯到早期的HTTP协议。虽然现今有更安全的认证方式（如OAuth 2.0），但因为简便性，基本认证仍广泛用于内网和不那么敏感的数据访问。重要的实现细节包括在请求头中包含一个经过Base64编码的用户名和密码组合，并以`Authorization: Basic`来加以前缀。

## 参见
- [HTTP基本认证规范(RFC7617)](https://tools.ietf.org/html/rfc7617)
- [Java网络编程概览](https://docs.oracle.com/javase/tutorial/networking/index.html)
- [Java中的Base64编码和解码](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [如何使用Java安全地存储密码](https://www.oracle.com/technical-resources/articles/javase/security.html)
