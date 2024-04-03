---
date: 2024-01-20 18:00:02.910583-07:00
description: "How to: \u600E\u4E48\u505A\uFF1F ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.622107-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

## How to: 怎么做？
```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("http://example.com"))
                .build();
        
        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
                .thenApply(HttpResponse::body)
                .thenAccept(System.out::println)
                .join();
    }
}
```
输出样例:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```

## Deep Dive 深度挖掘
HTTP请求的发送可以追溯到Web的早期时候。早期，Java使用`HttpURLConnection`，但这不太灵活且使用起来繁琐。Java 11引入了新的HTTP客户端API，它支持HTTP/2协议、同步和异步编程模型。与老式的`HttpURLConnection`相比，新API设计更为现代，易于使用，可以与Lambda表达式和Stream API结合起来使用。

替代方案包括Apache HttpClient、OkHttp和Retrofit。每个在特定场景下都有优势，例如Apache HttpClient功能强大但学习曲线较陡，OkHttp对Android优化良好，Retrofit则是适合REST API的高层抽象。

发送HTTP请求的实现细节需要注意的是连接管理、错误处理和性能问题。例如，维护HTTP连接池可以提高性能，正确处理HTTP响应码可以更好地管理程序逻辑。

## See Also 参考链接
- [Java 11 HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Apache HttpClient](https://hc.apache.org/httpcomponents-client-ga/)
- [OkHttp](https://square.github.io/okhttp/)
- [Retrofit](https://square.github.io/retrofit/)
