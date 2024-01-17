---
title:                "发送一个 http 请求"
html_title:           "Java: 发送一个 http 请求"
simple_title:         "发送一个 http 请求"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hello 揭金

## 什么是HTTP请求？为什么程序员要这么做？
HTTP请求是一种客户端向服务器发送请求的通信协议，它可以用来获取数据、发送表单数据等。程序员通常会使用HTTP请求来与服务器进行交互，获取所需的数据或实现特定的功能。

## 如何发送HTTP请求？
Java提供了许多方法来发送HTTP请求，其中最常用的是使用HttpURLConnection类。以下是一个简单的例子，演示如何使用HttpURLConnection类发送GET请求并获取服务器的响应数据：

```Java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public class HTTPRequestExample {

    public static void main(String[] args) throws Exception {

        // 创建URL对象
        URL url = new URL("https://www.example.com/api/users");

        // 打开和服务器的连接
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        // 设置请求方法为GET
        connection.setRequestMethod("GET");

        // 读取服务器的响应数据
        BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
        String inputLine;
        StringBuffer response = new StringBuffer();
        while ((inputLine = in.readLine()) != null) {
            response.append(inputLine);
        }
        in.close();

        // 打印服务器的响应数据
        System.out.println(response.toString());
    }
}
```

运行以上代码将输出服务器返回的JSON数据。

## 深入了解
*历史背景：HTTP请求最早是在1991年由蒂姆·伯纳斯·李提出的，用于分享文档和资源。
*其他方法：除了使用HttpURLConnection类，还有许多第三方库可以用来发送HTTP请求，例如Apache HttpComponents和OkHttp等。
*发送POST请求：如果要发送POST请求，可以使用HttpURLConnection类的setRequestMethod()方法将请求方法设置为POST，并使用getOutputStream()方法发送表单数据。
*实现细节：发送HTTP请求时，必须指定请求的URL、请求方法、请求头、请求体等信息，每个请求都会通过一个URLConnection对象来处理。

## 参考文献
更多关于Java发送HTTP请求的信息，可以参考以下文档：
- [Java HTTP请求教程](https://www.tutorialspoint.com/http/index.htm)
- [Java发送HTTP请求的代码示例](https://www.baeldung.com/java-http-request)
- [Java发送HTTP请求的官方文档](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/URLConnection.html)