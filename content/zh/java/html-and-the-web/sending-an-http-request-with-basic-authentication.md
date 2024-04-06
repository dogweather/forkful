---
date: 2024-01-20 18:01:57.120126-07:00
description: "\u5982\u4F55\uFF1A \u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684\
  HTTP\u8BF7\u6C42\u4E00\u76F4\u662F\u4E00\u4E2A\u8001\u751F\u5E38\u8C08\u7684\u8BDD\
  \u9898\u3002\u5B83\u7684\u5386\u53F2\u53EF\u8FFD\u6EAF\u5230\u65E9\u671F\u7684HTTP\u534F\
  \u8BAE\u3002\u867D\u7136\u73B0\u4ECA\u6709\u66F4\u5B89\u5168\u7684\u8BA4\u8BC1\u65B9\
  \u5F0F\uFF08\u5982OAuth\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.945447-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
