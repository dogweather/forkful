---
date: 2024-01-20 18:00:09.335708-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001\
  \u30B5\u30FC\u30D0\u30FC\u306BWeb\u30DA\u30FC\u30B8\u3084\u30B5\u30FC\u30D3\u30B9\
  \u3078\u306E\u554F\u3044\u5408\u308F\u305B\u3092\u884C\u3046\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u308C\u3092\u884C\u3046\u7406\
  \u7531\u306F\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u5916\
  \u90E8\u30B5\u30FC\u30D3\u30B9\u3068\u9023\u643A\u3059\u308B\u305F\u3081\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.302226-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001\u30B5\
  \u30FC\u30D0\u30FC\u306BWeb\u30DA\u30FC\u30B8\u3084\u30B5\u30FC\u30D3\u30B9\u3078\
  \u306E\u554F\u3044\u5408\u308F\u305B\u3092\u884C\u3046\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u308C\u3092\u884C\u3046\u7406\u7531\
  \u306F\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u5916\u90E8\
  \u30B5\u30FC\u30D3\u30B9\u3068\u9023\u643A\u3059\u308B\u305F\u3081\u3067\u3059\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

## What & Why? (何となぜ？)

HTTPリクエストを送るとは、サーバーにWebページやサービスへの問い合わせを行うことです。プログラマーがこれを行う理由は、データを取得したり、外部サービスと連携するためです。

## How to (やり方):

Luaでは、HTTPリクエストを送るために`socket.http`や`luasocket`モジュールを使うことが多いです。

```Lua
local http = require("socket.http")
local body, code, headers, status = http.request("http://www.example.com")

if code == 200 then
    print(body)  -- サーバーの応答を出力
else
    print(status)  -- エラー情報を出力
end
```

サンプル出力:

```
<!doctype html>...
```

これは`www.example.com`からHTMLを取得しています。

## Deep Dive (深掘り):

HTTPリクエストの送信方法は、インターネットの初期からあります。`socket.http`は基本的ですが、`HTTP/1.1`プロトコルの機能は全部サポートしていません。全機能が必要な場合は、`lua-requests`や`luasocket`ライブラリのようなもっと進んだライブラリを使うことがあります。

これらのライブラリは内部でTCP/IP接続を管理し、HTTPプロトコルを通じてデータを送受信します。`luasocket`ライブラリを使うと、HTTPSリクエストも送れますが、これには別途`ssl.https`モジュールが必要です。

## See Also (関連情報):

- LuaSocket公式サイト：http://w3.impa.br/~diego/software/luasocket/
- LuaSec (HTTPSサポート)：https://github.com/brunoos/luasec
- lua-requests：https://github.com/JakobGreen/lua-requests (シンプルなHTTPライブラリ)
