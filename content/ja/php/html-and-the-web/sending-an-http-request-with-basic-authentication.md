---
date: 2024-01-20 18:02:18.596952-07:00
description: "HTTP\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u3092\u7528\u3044\u305F\
  HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1\u3068\u306F\u3001\u30E6\u30FC\
  \u30B6\u30FC\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30BB\
  \u30AD\u30E5\u30A2\u306A\u30EA\u30BD\u30FC\u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3059\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\
  \u308C\u3092\u4F7F\u3063\u3066\u3001\u30D7\u30E9\u30A4\u30D9\u30FC\u30C8\u306AAPI\u3084\
  \u30EA\u30BD\u30FC\u30B9\u306B\u5B89\u5168\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\
  \u305F\u3081\u306B\u4F7F\u7528\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.245974-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u3092\u7528\u3044\u305FHTTP\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\
  \u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30BB\u30AD\u30E5\
  \u30A2\u306A\u30EA\u30BD\u30FC\u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\
  \u4F7F\u3063\u3066\u3001\u30D7\u30E9\u30A4\u30D9\u30FC\u30C8\u306AAPI\u3084\u30EA\
  \u30BD\u30FC\u30B9\u306B\u5B89\u5168\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\
  \u3081\u306B\u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

## What & Why? (なぜそれが必要か？)
HTTPベーシック認証を用いたHTTPリクエストの送信とは、ユーザー名とパスワードを使ってセキュアなリソースにアクセスすることです。プログラマーはこれを使って、プライベートなAPIやリソースに安全にアクセスするために使用します。

## How to: (やり方)
PHPでの基本的な認証を使ったHTTPリクエストの例です。

```PHP
<?php
$username = 'your_username';
$password = 'your_password';
$url = 'https://example.com/api/data';

$context = stream_context_create([
    'http' => [
        'header' => 'Authorization: Basic ' . base64_encode("$username:$password"),
        'method' => 'GET',
    ]
]);

$result = file_get_contents($url, false, $context);
if ($result !== false) {
    echo $result;
} else {
    echo 'Request failed.';
}
?>
```

このコードは、与えられたユーザー名とパスワードを使ってHTTP GETリクエストを送信し、結果を表示します。

## Deep Dive (詳細情報)
HTTPベーシック認証は、RFC7617で定義されており、インターネットの初期から使用されています。Basic認証はシンプルですが、ユーザー名とパスワードがBase64でエンコードされているだけなので、HTTPS経由での使用が推奨されます。

もっと安全性の高い代替手段としては、OAuthやAPIキーがあります。ただし、これらは実装が複雑になることがあります。

送信する際、`Authorization` ヘッダーにユーザー名とパスワードをBase64エンコードした値を付け加えます。`stream_context_create` 関数は、リクエストにオプションを設定するために使用されます。この方法で、PHPの`file_get_contents` 関数を用いてリモートコンテンツを取得することが可能になります。

## See Also (関連情報)
- PHP Manual on file_get_contents: https://www.php.net/manual/en/function.file-get-contents.php
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://datatracker.ietf.org/doc/html/rfc7617
- PHP Manual on stream_context_create: https://www.php.net/manual/en/function.stream-context-create.php
- Secure user authentication with OAuth: https://oauth.net/
