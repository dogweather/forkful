---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTP要求(with basic authentication)の送信は、特定の認証が必要なwebサーバーに情報を要求するための方法です。プログラマがこれを行う主な理由は、安全な情報交換を可能にするためです。

## こんな感じ：

LuaのHTTPライブラリは、HTTP要求を送信するための機能を提供します。以下はその一例です：

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

-- あなたの認証情報
local username = '認証ユーザ名'
local password = '認証パスワード'

-- URL
local url = 'http://your-url.com'

-- basic authentication 必要なヘッダーを設定
local headers = {
  ["Authorization"] = "Basic " .. (mime.b64(username .. ":" .. password))
}

-- Request送信
local response_body, status_code, headers, status_text = 
    http.request{
    url = url, 
    headers = headers
}

print(response_body)
print(status_code)
```

このスクリプトを実行すると、レスポンスが取得できます。

## 詳しく：

HTTPの基本認証は、初期のインターネットが開始された1990年頃から存在します。新しい認証スキーム（OAuthなど）が登場したものの、そのシンプルさと向後互換性のため、未だに広く使われています。

LuaでHTTP要求を送信する他の方法としては、luasocketを直接操作する、あるいは他のライブラリやフレームワークを使用する方法があります。しかし、http.requestはその少ないコード量と良好な評判のために、一般的には好まれる方法です。

このコードの具体的な実装については、基本認証用のヘッダー情報を指定するだけで、あとはhttp.requestが詳細を処理します。http.requestは、指定されたURLに対してGET方法でHTTPリクエストを送信し、その結果を返します。

## 参照：

以下に参考になるリンクをいくつか示します：

1. [LuaでのHTTPリクエスト送信](http://w3.impa.br/~diego/software/luasocket/http.html)
2. [LuaでのBasic Authenticationの使用](https://tools.ietf.org/html/rfc2617)  
3. [インターネットでの認証スキームの歴史](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
4. [Luaとその他のプログラミング言語でのHTTP要求の送信方法](https://realpython.com/python-requests/) 
5. [Luaの公式ドキュメンテーション](http://www.lua.org/manual/5.1/manual.html)