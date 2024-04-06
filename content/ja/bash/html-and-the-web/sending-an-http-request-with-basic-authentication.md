---
date: 2024-01-20 18:00:56.106836-07:00
description: "How to: (\u3084\u308A\u65B9) Bash\u3067\u57FA\u672C\u8A8D\u8A3C\u3092\
  \u4F34\u3046HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u306E\u306F`curl`\u30B3\
  \u30DE\u30F3\u30C9\u3092\u4F7F\u3048\u3070\u7C21\u5358\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.205422-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Bash\u3067\u57FA\u672C\u8A8D\u8A3C\u3092\u4F34\u3046\
  HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u306E\u306F`curl`\u30B3\u30DE\
  \u30F3\u30C9\u3092\u4F7F\u3048\u3070\u7C21\u5358\u3067\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

## How to: (やり方)
Bashで基本認証を伴うHTTPリクエストを送るのは`curl`コマンドを使えば簡単です。

```Bash
# ユーザー名'user'とパスワード'pass'を使ってGETリクエストを送る
curl -u user:pass http://example.com

# 上記のコマンドで生成されるサンプル出力
{"status":"success","message":"Authenticated"}

# POSTリクエストを送る場合（データを含む)
curl -u user:pass -d "param1=value1&param2=value2" -X POST http://example.com

# 上記のコマンドで生成されるサンプル出力
{"status":"success","message":"Data received"}
```

`-u` オプションは基本認証を行うためのユーザー名とパスワードを指定するのに使います。結果はサーバーのレスポンスに依存します。

## Deep Dive (深掘り)
基本認証はHTTPプロトコルの標準的な方法で、RFC 7617で定義されています。ユーザー名とパスワードは`:`で連結され、Base64エンコードされて送信されます。

しかし、安全ではないネットワークではBase64エンコーディングは易しく解読されるため、HTTPSを通じた通信が推奨されます。

基本認証の代わりにOAuthなどのより安全な認証方法もあります。これらはトークンを使用して認証を行い、クレデンシャルを毎回送らなくても済むので安全です。`curl`コマンドはこれらの認証方法もサポートしています。

実装の詳細では、`curl`は`.netrc`ファイルを使用してクレデンシャル情報を管理し、より安全に保つことができます。`.netrc`ファイルにはサーバーのアドレス、ユーザー名、パスワードを入力しておくことで、コマンドで毎回指定しなくても利用できます。

## See Also (関連情報)
- cURLの公式ドキュメント: https://curl.haxx.se/docs/manpage.html
- HTTP基本認証のRFC 7617文書: https://tools.ietf.org/html/rfc7617
- HTTPSとセキュリティに関するMozilla開発者ネットワーク (MDN) の記事: https://developer.mozilla.org/en-US/docs/Web/Security
- OAuthについての詳細: https://oauth.net/
