---
date: 2024-01-20 18:02:28.483490-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u57FA\u672C\u8A8D\u8A3C\u3067\
  \u9001\u308B\u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u306B\u5B89\u5168\u306B\u30C7\
  \u30FC\u30BF\u3092\u9001\u3063\u305F\u308A\u53D6\u3063\u305F\u308A\u3059\u308B\u65B9\
  \u6CD5\u3067\u3059\u3002\u3053\u308C\u306F\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\
  \u3068\u901A\u4FE1\u3059\u308B\u3068\u304D\u306B\u3001\u30E6\u30FC\u30B6\u30FC\u540D\
  \u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u8EAB\u5143\u3092\u78BA\
  \u8A8D\u3059\u308B\u305F\u3081\u306B\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.307483-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u57FA\u672C\u8A8D\u8A3C\u3067\u9001\
  \u308B\u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u306B\u5B89\u5168\u306B\u30C7\u30FC\
  \u30BF\u3092\u9001\u3063\u305F\u308A\u53D6\u3063\u305F\u308A\u3059\u308B\u65B9\u6CD5\
  \u3067\u3059\u3002\u3053\u308C\u306F\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\u3068\
  \u901A\u4FE1\u3059\u308B\u3068\u304D\u306B\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\
  \u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u8EAB\u5143\u3092\u78BA\u8A8D\
  \u3059\u308B\u305F\u3081\u306B\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
