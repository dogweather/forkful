---
date: 2024-01-20 18:00:56.106836-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\
  \u884C\u3046\u3053\u3068\u3067\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\u30B9\
  \u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30B5\u30FC\u30D0\u30FC\u3078\u306E\u5B89\
  \u5168\u306A\u30A2\u30AF\u30BB\u30B9\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\
  \u3053\u306E\u65B9\u6CD5\u306F\u3001API\u3084\u30EA\u30E2\u30FC\u30C8\u30EA\u30BD\
  \u30FC\u30B9\u306B\u5BFE\u3059\u308B\u64CD\u4F5C\u3092\u81EA\u52D5\u5316\u3059\u308B\
  \u969B\u306B\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.369737-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\u884C\
  \u3046\u3053\u3068\u3067\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\u30B9\u30EF\
  \u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30B5\u30FC\u30D0\u30FC\u3078\u306E\u5B89\u5168\
  \u306A\u30A2\u30AF\u30BB\u30B9\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u3053\
  \u306E\u65B9\u6CD5\u306F\u3001API\u3084\u30EA\u30E2\u30FC\u30C8\u30EA\u30BD\u30FC\
  \u30B9\u306B\u5BFE\u3059\u308B\u64CD\u4F5C\u3092\u81EA\u52D5\u5316\u3059\u308B\u969B\
  \u306B\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002."
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
