---
date: 2024-01-20 18:02:06.571224-07:00
description: "How to: (\u3084\u308A\u65B9) HTTP\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\
  \u8A3C\u306F\u53E4\u5178\u7684\u306A\u8A8D\u8A3C\u306E\u4E00\u3064\u3067\u3001RFC\
  \ 7617\u306B\u3088\u3063\u3066\u6A19\u6E96\u5316\u3055\u308C\u307E\u3057\u305F\u3002\
  \u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u304C\u5F31\u3044\u305F\u3081\u3001HTTPS\u3067\
  \u306E\u4F7F\u7528\u304C\u63A8\u5968\u3055\u308C\u3066\u3044\u307E\u3059\u3002OAuth\u3084\
  JWT\u306A\u3069\u3001\u3088\u308A\u30BB\u30AD\u30E5\u30A2\u306A\u8A8D\u8A3C\u65B9\
  \u6CD5\u3082\u3042\u308A\u307E\u3059\u304C\u3001\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\
  \u8A3C\u306F\u30B7\u30F3\u30D7\u30EB\u3067\u5B9F\u88C5\u304C\u5BB9\u6613\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:55.894590-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) HTTP\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u306F\
  \u53E4\u5178\u7684\u306A\u8A8D\u8A3C\u306E\u4E00\u3064\u3067\u3001RFC 7617\u306B\
  \u3088\u3063\u3066\u6A19\u6E96\u5316\u3055\u308C\u307E\u3057\u305F\u3002\u30BB\u30AD\
  \u30E5\u30EA\u30C6\u30A3\u304C\u5F31\u3044\u305F\u3081\u3001HTTPS\u3067\u306E\u4F7F\
  \u7528\u304C\u63A8\u5968\u3055\u308C\u3066\u3044\u307E\u3059\u3002OAuth\u3084JWT\u306A\
  \u3069\u3001\u3088\u308A\u30BB\u30AD\u30E5\u30A2\u306A\u8A8D\u8A3C\u65B9\u6CD5\u3082\
  \u3042\u308A\u307E\u3059\u304C\u3001\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u306F\
  \u30B7\u30F3\u30D7\u30EB\u3067\u5B9F\u88C5\u304C\u5BB9\u6613\u3067\u3059\u3002"
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
