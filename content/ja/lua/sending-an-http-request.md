---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T18:00:09.335708-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

category:             "Lua"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

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
