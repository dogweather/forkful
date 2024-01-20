---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么以及为何？
HTTP请求只是一个互联网上的消息，它发送给特定的服务器来获取或提交数据。这个功能在编程中非常重要，因为它允许不同的应用程序或服务间进行通信和数据交换。

## 怎么做：
Java内置包java.net.http作为HTTP客户端，用来发送HTTP请求。下面是我们的一个例子。

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpClientExample {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
              .uri(URI.create("http://example.com/"))
              .build();

        HttpResponse<String> response =
              client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}
```
输出:
```
<html>
<head>
<title>Example Domain</title>
...
</html>
```

## 深度探讨：
在Java 11之前，JDK没有提供官方的HTTP客户端。这迫使许多开发者转向第三方库，例如Apache HttpClient等。Java 11更新了内置HTTP客户端，使其更具现代性，支持HTTP/2，并异步或同步处理请求和响应。

虽然Java的内置HTTP客户端非常实用，但当你需要更复杂功能（如代理设置、Cookies管理和多种身份验证方式）时，Apache HttpClient可能是更好的选择。

至于实施细节，当使用内置HTTP客户端发送POST或PUT请求时，你需要创建一个HttpRequest.BodyPublisher实例来发送请求体。你可以使用HttpRequest.BodyPublishers类中的工厂方法来创建。

## 归纳参考：
- Oracle官方Java HTTP客户端教程: http://docs.oracle.com/en/java/javase/11/net/httpclient-overview.html
- Apache HttpClient库：https://hc.apache.org/httpcomponents-client-4.5.x/index.html
- Java 11 HttpClient的异步示例：https://www.callicoder.com/java-11-http-client-api-tutorial-examples/