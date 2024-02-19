---
aliases:
- /ja/java/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:06.571224-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\
  \u8A8D\u8A3C\u3092\u4ED8\u3051\u3066\u9001\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\
  \u30FC\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u8A8D\u8A3C\
  \u3092\u884C\u3046\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u3053\u308C\u3092\u884C\
  \u3046\u7406\u7531\u306F\u3001API\u3084\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\
  \u306B\u5B89\u5168\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.801507
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\
  \u8A3C\u3092\u4ED8\u3051\u3066\u9001\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\
  \u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u8A8D\u8A3C\u3092\
  \u884C\u3046\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u3053\u308C\u3092\u884C\u3046\
  \u7406\u7531\u306F\u3001API\u3084\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\u306B\
  \u5B89\u5168\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u3067\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (それは何？どうして？)

HTTPリクエストにベーシック認証を付けて送るとは、ユーザー名とパスワードを使って認証を行うプロセスです。これを行う理由は、APIやウェブサービスに安全にアクセスするためです。

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
