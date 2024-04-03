---
date: 2024-01-20 18:02:06.571224-07:00
description: "How to: (\u3084\u308A\u65B9) \u307E\u305A\u306F\u5FC5\u8981\u306A\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30AF\u30EB\u30FC\u30C9\u3057\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.946800-06:00'
model: gpt-4-1106-preview
summary: "\u307E\u305A\u306F\u5FC5\u8981\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u30A4\u30F3\u30AF\u30EB\u30FC\u30C9\u3057\u307E\u3059."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

## How to: (やり方)
まずは必要なライブラリをインクルードします。

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Base64;
```

次に、ベーシック認証のためにユーザー名とパスワードをエンコードします。

```java
String username = "user";
String password = "pass";
String encodedCredentials = Base64.getEncoder().encodeToString((username + ":" + password).getBytes());
```

HttpClient を作成し、リクエストに認証ヘッダを設定して送信します。

```java
HttpClient client = HttpClient.newHttpClient();
HttpRequest request = HttpRequest.newBuilder()
        .uri(URI.create("http://example.com"))
        .header("Authorization", "Basic " + encodedCredentials)
        .build();

client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
        .thenApply(HttpResponse::body)
        .thenAccept(System.out::println)
        .join();
```

送信されるHTTPリクエストはこれで完了です。

## Deep Dive (詳細解説)
HTTPベーシック認証は古典的な認証の一つで、RFC 7617によって標準化されました。セキュリティが弱いため、HTTPSでの使用が推奨されています。OAuthやJWTなど、よりセキュアな認証方法もありますが、ベーシック認証はシンプルで実装が容易です。

## See Also (関連情報)
- HTTPベーシック認証に関する更なる情報はRFC 7617をご覧ください。 (https://tools.ietf.org/html/rfc7617)
- JavaのHttpClientの詳細なドキュメントはオラクルの公式文書をチェックしてください。 (https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)

この記事を参考に、JavaでHTTPリクエストにベーシック認証を行ってみてください。
