---
date: 2024-01-20 18:00:02.910583-07:00
description: "\u53D1\u9001HTTP\u8BF7\u6C42\u5C31\u662F\u8BA9\u7A0B\u5E8F\u901A\u8FC7\
  \u7F51\u7EDC\u5411\u670D\u52A1\u5668\u8BF7\u6C42\u6570\u636E\u6216\u53D1\u9001\u6570\
  \u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\u8FD9\u6837\u53EF\
  \u4EE5\u4ECEWeb\u670D\u52A1\u83B7\u53D6\u4FE1\u606F\uFF0C\u6267\u884C\u64CD\u4F5C\
  \uFF0C\u6216\u4E0E\u5176\u4ED6\u7CFB\u7EDF\u8FDB\u884C\u4EA4\u4E92\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.395200-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001HTTP\u8BF7\u6C42\u5C31\u662F\u8BA9\u7A0B\u5E8F\u901A\u8FC7\u7F51\
  \u7EDC\u5411\u670D\u52A1\u5668\u8BF7\u6C42\u6570\u636E\u6216\u53D1\u9001\u6570\u636E\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\u8FD9\u6837\u53EF\u4EE5\
  \u4ECEWeb\u670D\u52A1\u83B7\u53D6\u4FE1\u606F\uFF0C\u6267\u884C\u64CD\u4F5C\uFF0C\
  \u6216\u4E0E\u5176\u4ED6\u7CFB\u7EDF\u8FDB\u884C\u4EA4\u4E92\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
发送HTTP请求就是让程序通过网络向服务器请求数据或发送数据。程序员这么做是因为这样可以从Web服务获取信息，执行操作，或与其他系统进行交互。

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
