---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:00:56.106836-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (なぜとは?)
HTTPリクエストに基本認証を行うことで、ユーザー名とパスワードを使ってサーバーへの安全なアクセスを可能にします。この方法は、APIやリモートリソースに対する操作を自動化する際によく使われます。

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
