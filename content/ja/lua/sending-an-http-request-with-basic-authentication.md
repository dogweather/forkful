---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "Lua: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何故＆何？
HTTPリクエストを基本認証で送信するとは、ウェブサービスへのアクセスに使用されるセキュリティの仕組みです。プログラマーは、この方法を使用して、ウェブサービスにアクセスし、ユーザーの認証情報を確認することができます。

## 手順：
```
-- LuaでHTTPリクエストを送信するための基本認証のコード例
local http = require("socket.http")

-- ウェブサービスのURLと認証情報を設定
local url = "https://example.com/api"
local username = "ユーザー名"
local password = "パスワード"

-- HTTPリクエストを作成
local request = {
  method = "GET",
  url = url,
  headers = {
    authorization = "Basic " ..  -- HTTPリクエストヘッダーに認証情報を追加
      mime.b64(username .. ":" .. password)
  }
}

-- HTTPリクエストを送信し、レスポンスを取得
local response = http.request(request)

-- レスポンスを表示
print(response)
```

上記のコードでは、```Lua ... ```を使用してbasic認証を追加し、HTTPリクエストを送信します。また、socket.httpを使用することで、リクエストを作成し、レスポンスを取得します。

## 深堀り：
基本認証は、HTTPプロトコルの初期のバージョンであるHTTP / 1.0で使用されるように導入されました。現在、より安全な認証方法であるOAuthが広く使用されていますが、基本認証はまだ多くのウェブサービスで使用されています。

他の代替方法としては、クライアント証明書を使用したSSL認証などがあります。また、上記のコードでは、HTTPリクエストを送信するためのライブラリとしてsocket.httpを使用しましたが、代わりにLuaのHTTPクライアントであるluasocketを使用することもできます。

## 関連リンク：
- [LuaでHTTPリクエストを送信する方法](https://www.lua.org/pil/22.3.html)
- [HTTPクライアントライブラリluasocketのドキュメント](http://w3.impa.br/~diego/software/luasocket/http.html)