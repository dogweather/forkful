---
title:                "HTTPリクエストの送信"
html_title:           "Lua: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何？　なんで？

HTTPリクエストを送るとはどういうことかを、2〜3文で説明します。また、プログラマーがなぜそのようなことをするのかも説明します。

HTTPリクエストを送るとは、インターネット上の他のコンピュータにデータを要求することです。例えば、ウェブページを閲覧する際に、HTTPリクエストを送ってサーバーからデータを取得します。プログラマーは、自分のプログラムと他のシステム間でデータをやり取りする際に、HTTPリクエストを利用することがあります。

## 使い方

下のCode blockを参考にして、LuaでHTTPリクエストを送る方法を確かめてみましょう。

```Lua
-- 必要なライブラリを読み込み
local http = require("socket.http")

-- URLとリクエストタイプを定義
local url = "https://example.com/api/users"
local method = "GET"

-- リクエストを送信
local body, code, headers = http.request(url, method)

-- リクエストが成功したかチェック
if code == 200 then
    -- レスポンスボディを出力
    print(body)
else 
    -- エラーコードを出力
    print("Error code: " .. code)
end 
```

上のコードでは、[Luasocket](https://github.com/diegonehab/luasocket)というライブラリを利用してHTTPリクエストを送っています。このライブラリには、リクエストボディを作成したり、レスポンスヘッダーを取得するための便利な関数が含まれています。

## 深堀り

- HTTPリクエストは、Webの通信プロトコルであるHTTPの基本的な機能の一つです。HTTPは、1990年代に世界初のWebサーバーである[NCSA HTTPd](https://en.wikipedia.org/wiki/NCSA_HTTPd)を開発したティム・バーナーズ＝リーによって策定されました。
- Lua以外にも、PythonやNode.jsなど様々なプログラミング言語でHTTPリクエストを送ることができます。それぞれの言語によって、実装方法やライブラリの選択方法が異なりますので、自分のプロジェクトに最適なものを選びましょう。
- HTTPリクエストを送る際には、本文以外にリクエストヘッダーと呼ばれる情報も併せて送ることができます。ヘッダーには、コンテンツのタイプやセキュリティなどの情報が含まれています。

## 関連リンク

- [Luasocket公式ドキュメント](http://w3.impa.br/~diego/software/luasocket/)
- [HTTPリクエストとは？ | MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- [HTTPプロトコルについて学ぼう | Qiita](https://qiita.com/takeshiyako2/items/c0807a44341d55c33a7a)