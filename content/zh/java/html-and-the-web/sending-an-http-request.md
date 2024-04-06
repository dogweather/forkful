---
date: 2024-01-20 18:00:02.910583-07:00
description: "How to: \u600E\u4E48\u505A\uFF1F HTTP\u8BF7\u6C42\u7684\u53D1\u9001\u53EF\
  \u4EE5\u8FFD\u6EAF\u5230Web\u7684\u65E9\u671F\u65F6\u5019\u3002\u65E9\u671F\uFF0C\
  Java\u4F7F\u7528`HttpURLConnection`\uFF0C\u4F46\u8FD9\u4E0D\u592A\u7075\u6D3B\u4E14\
  \u4F7F\u7528\u8D77\u6765\u7E41\u7410\u3002Java\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.783446-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A\uFF1F HTTP\u8BF7\u6C42\u7684\u53D1\u9001\u53EF\u4EE5\u8FFD\
  \u6EAF\u5230Web\u7684\u65E9\u671F\u65F6\u5019\u3002\u65E9\u671F\uFF0CJava\u4F7F\u7528\
  `HttpURLConnection`\uFF0C\u4F46\u8FD9\u4E0D\u592A\u7075\u6D3B\u4E14\u4F7F\u7528\u8D77\
  \u6765\u7E41\u7410\u3002Java 11\u5F15\u5165\u4E86\u65B0\u7684HTTP\u5BA2\u6237\u7AEF\
  API\uFF0C\u5B83\u652F\u6301HTTP/2\u534F\u8BAE\u3001\u540C\u6B65\u548C\u5F02\u6B65\
  \u7F16\u7A0B\u6A21\u578B\u3002\u4E0E\u8001\u5F0F\u7684`HttpURLConnection`\u76F8\u6BD4\
  \uFF0C\u65B0API\u8BBE\u8BA1\u66F4\u4E3A\u73B0\u4EE3\uFF0C\u6613\u4E8E\u4F7F\u7528\
  \uFF0C\u53EF\u4EE5\u4E0ELambda\u8868\u8FBE\u5F0F\u548CStream API\u7ED3\u5408\u8D77\
  \u6765\u4F7F\u7528\u3002"
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
