---
date: 2024-01-20 18:02:28.483490-07:00
description: "How to (\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u306F`socket.http`\u3068\
  `ltn12`\u3092\u4F7F\u3063\u3066HTTP GET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\
  \u308A\u307E\u3059\u3002\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\u30B9\u30EF\u30FC\
  \u30C9\u3067\u8A8D\u8A3C\u3057\u3066\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.830573-06:00'
model: gpt-4-1106-preview
summary: "How to (\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u306F`socket.http`\u3068\
  `ltn12`\u3092\u4F7F\u3063\u3066HTTP GET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\
  \u308A\u307E\u3059\u3002\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\u30B9\u30EF\u30FC\
  \u30C9\u3067\u8A8D\u8A3C\u3057\u3066\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\
  \u307E\u3059\u3002"
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
