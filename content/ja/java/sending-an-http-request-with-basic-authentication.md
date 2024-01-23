---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:02:06.571224-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/sending-an-http-request-with-basic-authentication.md"
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
