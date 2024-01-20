---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ?
Webページのダウンロードとは、インターネット上に存在するページの全てのデータを使用者のコンピューターに保存することです。これにより、プログラマはブログポストを取得したり、サービスにデータをフィードしたり、ウェブスクレイピングやデータ分析を行うのに役立てます。

## 使い方:
Luaでwebページをダウンロードするには、一部の外部ライブラリが必要です。`lua-socket`や`lua-socket-http`が一例です。以下にそのコードを示します。

```Lua
-- 必要なライブラリを読み込む
http = require("socket.http")

-- WebページのURL
url = "https://www.example.com"

-- ダウンロードと保存
body, code, headers, status = http.request(url)

if code == 200 then
    -- 正常にダウンロードされた場合、内容を表示
    print(body)
else
    -- 何らかの問題が発生した場合、ステータスコードを表示
    print("HTTP request failed with status: ", status)
end
```
このコードは、指定したURLからウェブページをダウンロードして結果を表示します。成功した場合、ウェブページのHTMLが出力され、何らかのエラーが発生した場合は、HTTPステータスコードとエラーメッセージが表示されます。

## 深堀り:
Webページをダウンロードするだけでなく、取得したHTMLを解析するために`lua-htmlparser`のようなライブラリも利用できます。こういったダウンローダは、初期のインターネット時代から存在し、ウェブクローラーやスクレイパーの根幹となっています。また、Lua以外のプログラミング言語でも類似したライブラリやツールが多数存在します（例えばPythonの`requests`や`BeautifulSoup`など）。

## 参考リンク:
- Luaの公式ドキュメント: http://www.lua.org/manual/5.4/
- lua-socketの公式GitHubページ: https://github.com/diegonehab/luasocket
- lua-htmlparserの公式GitHubページ: https://github.com/msva/lua-htmlparser