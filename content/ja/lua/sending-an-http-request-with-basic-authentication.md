---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:02:28.483490-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (なに？ どうして？)
HTTPリクエストを基本認証で送るのは、サーバーに安全にデータを送ったり取ったりする方法です。これはウェブサービスと通信するときに、ユーザー名とパスワードを使って身元を確認するためによく使われます。

## How to (やり方)
```Lua
-- Luaには標準HTTPライブラリがないので、socket.httpを使います。
local http = require("socket.http")
local ltn12 = require("ltn12")

-- ベーシック認証の準備
local username = "your_username"
local password = "your_password"
local auth = "Basic " .. (mime.b64(username .. ":" .. password))

-- HTTPリクエストの設定
local response_body = {}
local res, code, response_headers = http.request{
  url = "http://example.com/data",
  method = "GET",
  headers = 
  {
    ["Authorization"] = auth
  },
  sink = ltn12.sink.table(response_body)
}

-- 結果のチェック
if code == 200 then
  print("Success:")
  print(table.concat(response_body))
else
  print("Error:", code)
end

-- 出力例 (成功時)
-- Success:
-- {"data": "この中にあなたがリクエストしたデータが含まれます"}
```

このコードは`socket.http`と`ltn12`を使ってHTTP GETリクエストを送ります。ユーザー名とパスワードで認証して、データを取得します。

## Deep Dive (深掘り)
HTTPリクエストの基本認証は、ウェブが始まって以来の古い方法です。暗号化されていないので、HTTPS経由で使うのが一般的です。 

代替方法としては、OAuthなどがありますが、設定が複雑な場合があります。基本認証の場合、ユーザー名とパスワードをBase64でエンコードしてヘッダに含めます。このシンプルさが、特に内部のシステムやシンプルなAPIとの連携ではまだ利用されています。

詳細実装においては`socket.http`を使い、`lua-sec`を組み合わせることでHTTPSリクエストも行えます。`ltn12.sink`はレスポンスボディを受け取るために利用しています。

## See Also (関連情報)
- LuaSocket documentation: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec for HTTPS requests: https://github.com/brunoos/luasec/wiki
- Basic Authentication on Wikipedia: https://ja.wikipedia.org/wiki/Basic%E8%AA%8D%E8%A8%BC