---
date: 2024-01-20 18:01:57.120126-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u662F\u4E00\u79CD\u901A\u8FC7\u7F51\u7EDC\u5411\u670D\u52A1\u5668\u9A8C\u8BC1\u8EAB\
  \u4EFD\u7684\u65B9\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\
  \u4E3A\u4E86\u5B89\u5168\u5730\u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u8D44\u6E90\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.649126
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u662F\
  \u4E00\u79CD\u901A\u8FC7\u7F51\u7EDC\u5411\u670D\u52A1\u5668\u9A8C\u8BC1\u8EAB\u4EFD\
  \u7684\u65B9\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\u4E3A\
  \u4E86\u5B89\u5168\u5730\u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u8D44\u6E90\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
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
