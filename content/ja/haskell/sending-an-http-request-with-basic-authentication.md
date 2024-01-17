---
title:                "基本認証を使用してhttpリクエストを送信する方法"
html_title:           "Haskell: 基本認証を使用してhttpリクエストを送信する方法"
simple_title:         "基本認証を使用してhttpリクエストを送信する方法"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 何かとは？
Haskellのプログラマーにとって、HTTPリクエストを基本認証で送信するというのはなにか？というと、基本認証はHTTPリクエストを行う際に、ユーザーの認証を行うためのものです。プログラマーはセキュリティを保つために基本認証を使用します。

# 方法：
```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.HTTP.Client.Auth

-- リクエスト作成
request <- parseRequest "https://example.com"
-- リクエストに基本認証を追加
let request' = applyBasicAuth "username" "password" request
-- リクエストを送信
response <- httpLbs request' manager
-- レスポンスのステータスコードを確認
let status = responseStatus response
-- レスポンスのボディを取得
let body = responseBody response
-- レスポンスの表示
print "ステータスコード：" ++ show status
print "ボディ: " ++ show body
```

実行結果：
```
ステータスコード：200
ボディ: "Hello World!"
```


# 詳しく調べる
基本認証は1999年に導入されました。これは、HTTPプロトコルのセキュリティを強化するためのものでした。基本認証以外の認証方法としては、ダイジェスト認証やOAuthがあります。基本認証は、パスワード保護されたリソースにアクセスする必要がある場合に使用されます。HTTPリクエストを送信する際に、プログラマーはユーザーの認証情報を提供することにより、基本認証を行います。

# 関連情報を参照
- [HTTP Basics](https://www.w3.org/Protocols/rfc2616/rfc2616-sec8.html)
- [HttpClient - Haskell package](https://hackage.haskell.org/package/http-client)
- [HTTP Authentication: Basic and Digest Access Authentication](https://no.wikipedia.org/wiki/HTTP-بعثة:基本及びダイジェストアクセス認証)