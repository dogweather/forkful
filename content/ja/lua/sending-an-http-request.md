---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストの送信はWebサーバーに対する要求の送信を指します。これはWebサイトのコンテンツを取得したり、データを更新したりするためにプログラマーが使用します。

## 使い方：

Luaを用いてHTTPリクエストを送信する例を以下に示します。

```Lua
http = require('socket.http')

url = 'http://httpbin.org/post'

response_body, response_status, response_headers, response_status_line = http.request{
  url = url,
  method = 'POST',
  headers = {
    ['Content-Type'] = 'application/x-www-form-urlencoded'
  },
  source = ltn12.source.string('key=value'),
  sink = ltn12.sink.table(respbody)
}

print(response_status)  -- 出力：200
```
このコードは`http://httpbin.org/post`に`POST`リクエストを送信します。レスポンスステータスは`200`を出力します。

## 深掘り：

Webプログラミングの初期では、HTTPリクエストは手動ですべてのヘッダーとともに構築し、送信する必要がありました。しかし、現在ではバージョン5.1から共有ライブラリとして利用できるLuaSocketライブラリのようなツールにより、このプロセスが大幅に簡素化されました。

Lua以外にも多くの言語でHTTPリクエストを送信する方法が存在します。Pythonでは`requests`ライブラリ、JavaScriptでは`fetch`APIが一般的です。

LuaではHTTPリクエストを送信するためにコルーチンを使用することも可能です。これは非同期操作を行うための方法で、プログラムの他の部分がレスポンスを待つ間に実行を続けることができます。

## 参考リンク：

1. [LuaSocket Tutorial](http://w3.impa.br/~diego/software/luasocket/tutorial.html): チュートリアルではLuaSocketを用いたより詳細なネットワークプログラミングについて解説しています。

2. [HTTP made really easy](http://www.jmarshall.com/easy/http/): HTTPプロトコルの基本から詳細まで説明している資料です。

3. [Lua JIT](https://luajit.org/): LuaのJITコンパイラの公式ページ。Luaのパフォーマンスを向上させるためのツールです。