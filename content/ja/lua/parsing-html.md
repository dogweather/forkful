---
title:                "HTMLの解析"
date:                  2024-01-20T15:32:59.987359-07:00
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTMLをパース（解析）するのは、ウェブページの構造を理解してデータにアクセスすることです。プログラマーはデータ抽出、自動化、コンテンツマイニングなどのためにこれを行います。

## How to: (どうやって？)
Luaでは、外部ライブラリを利用してHTMLをパースします。`lua-html`は使いやすい選択です。

```lua
local html = require("html.parser")
local page = [[
<html>
  <head>
    <title>Sample Page</title>
  </head>
  <body>
    <h1>Hello, World!</h1>
    <p>This is a sample paragraph.</p>
  </body>
</html>
]]

local dom = html.parse(page)

-- タイトルを取得
local titles = dom:select("title")
for i, title in ipairs(titles) do
    print(title:getcontent())
end

-- 結果: Sample Page
```

## Deep Dive (深掘り)
HTMLパースは1990年代初頭から行われています。LynxブラウザなどがテキストモードでHTMLを表示するために最初にこれを行いました。Luaでは標準ライブラリにHTMLパーサーは含まれておらず、`lua-html`や`Gumbo`などのサードパーティ・ライブラリがあります。これらはDOMを解析し易い形で提供し、CSSセレクタで要素を簡単に抽出できます。パフォーマンスと正確性はライブラリによって異なります。

## See Also (関連情報)
- lua-html: https://github.com/msva/lua-html
- Gumbo parser: https://github.com/craigbarnes/lua-gumbo
- Lua 5.4 reference manual: https://www.lua.org/manual/5.4/
- HTML5 parsing algorithm: https://html.spec.whatwg.org/multipage/parsing.html

HTMLのパースには、正しいツールと知識が必要です。最適なライブラリを選び、素晴らしいLuaプログラミングを！
