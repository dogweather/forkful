---
aliases:
- /ja/java/sending-an-http-request/
date: 2024-01-20 18:00:14.742611-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3063\u3066\u3044\u3046\u306E\u306F\
  \u3001Web\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\u6C42\u3059\u308B\
  \u65B9\u6CD5\u3067\u3059\u3002\u3053\u306E\u6280\u8853\u3092\u4F7F\u3046\u7406\u7531\
  \u306F\u3001\u5916\u90E8\u30C7\u30FC\u30BF\u306E\u53D6\u5F97\u3084API\u3068\u306E\
  \u9023\u643A\u306E\u305F\u3081\u3002\u7C21\u5358\u304B\u3064\u5F37\u529B\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.798850
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3063\u3066\u3044\u3046\u306E\u306F\u3001\
  Web\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\u6C42\u3059\u308B\u65B9\
  \u6CD5\u3067\u3059\u3002\u3053\u306E\u6280\u8853\u3092\u4F7F\u3046\u7406\u7531\u306F\
  \u3001\u5916\u90E8\u30C7\u30FC\u30BF\u306E\u53D6\u5F97\u3084API\u3068\u306E\u9023\
  \u643A\u306E\u305F\u3081\u3002\u7C21\u5358\u304B\u3064\u5F37\u529B\u3067\u3059\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

HTTPリクエストっていうのは、Webサーバーに情報を要求する方法です。この技術を使う理由は、外部データの取得やAPIとの連携のため。簡単かつ強力です。

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
