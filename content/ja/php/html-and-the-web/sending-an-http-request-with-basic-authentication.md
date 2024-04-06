---
date: 2024-01-20 18:02:18.596952-07:00
description: "How to: (\u3084\u308A\u65B9) PHP\u3067\u306E\u57FA\u672C\u7684\u306A\
  \u8A8D\u8A3C\u3092\u4F7F\u3063\u305FHTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u4F8B\
  \u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.099267-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) PHP\u3067\u306E\u57FA\u672C\u7684\u306A\u8A8D\u8A3C\
  \u3092\u4F7F\u3063\u305FHTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u4F8B\u3067\u3059\
  \u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
