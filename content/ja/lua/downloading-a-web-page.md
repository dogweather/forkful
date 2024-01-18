---
title:                "ウェブページのダウンロード"
html_title:           "Lua: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なに & なぜ？

ウェブページをダウンロードすることは、インターネット上の情報を取得することです。プログラマーがウェブページをダウンロードする理由は、ウェブ上のデータを取得し、処理することでさまざまなアプリケーションを開発することができるからです。

## 方法：

```Lua
-- ウェブページをダウンロードするためのライブラリをインポート
local http = require("socket.http")

-- 下記のURLからウェブページをダウンロード
local url = "https://www.test.com"
local body, status_code, headers = http.request(url)

-- ダウンロードが成功したかどうかをチェック
if status_code == 200 then 
    -- ウェブページの内容を出力
    print(body) 
else 
    -- ダウンロードが失敗した場合、エラーを表示
    print("Error downloading webpage: " .. status_code) 
end
```

```
#　サンプル出力：
<html>
<head>
<title>Test Page</title>
</head>
<body>
<h1>Welcome to Test Page!</h1>
</body>
</html>
```

## 深く掘り下げる：

ウェブページをダウンロードするライブラリは、Luaプログラミング言語の標準ライブラリで使用可能です。ただし、オプションのライブラリを使用することもできます。ダウンロード中にプログラムがブロックされることを防ぐために、非同期ダウンロードを行うこともできます。

## 関連リンク：

- Luaのウェブページダウンロードのドキュメント：https://www.lua.org/manual/5.4/manual.html#6.4 
- socket.httpライブラリのドキュメント：https://w3.impa.br/~diego/software/luasocket/http.html 
- 非同期ダウンロードのライブラリ：https://github.com/brimworks/lua-http