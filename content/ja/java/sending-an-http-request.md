---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T18:00:14.742611-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

category:             "Java"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/sending-an-http-request.md"
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
