---
date: 2024-01-20 18:00:14.742611-07:00
description: "How to: (\u3084\u308A\u65B9) \u4EE5\u4E0B\u306E\u30B5\u30F3\u30D7\u30EB\
  \u30B3\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u3001Java\u3067HTTP GET\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u3092\u9001\u308B\u65B9\u6CD5\u3092\u898B\u3066\u307F\u307E\u3057\u3087\
  \u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.838565-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u4EE5\u4E0B\u306E\u30B5\u30F3\u30D7\u30EB\u30B3\u30FC\
  \u30C9\u3092\u4F7F\u3063\u3066\u3001Java\u3067HTTP GET\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u9001\u308B\u65B9\u6CD5\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

## How to: (やり方)
以下のサンプルコードを使って、JavaでHTTP GETリクエストを送る方法を見てみましょう。

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class Main {
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

サンプル出力:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (深く掘り下げて)
かつてJavaでは、`HttpURLConnection` クラスを使ってHTTPリクエストを送っていましたが、Java 11からは `java.net.http.HttpClient` クラスが導入され、よりシンプルかつモダンなAPIとして推奨されています。他の言語のライブラリと同様に、非同期処理もサポートされています。`HttpClient`の使用により、HTTP/2のサポートやWebSocketなど、他にも強力な機能を利用できます。

## See Also (関連情報)
- [Oracleの公式ドキュメンテーション](https://docs.oracle.com/en/java/javase/17/docs/api/java.net.http/java/net/http/HttpClient.html)
- [HTTPリクエストのバリエーションを理解する](https://www.baeldung.com/java-9-http-client)
