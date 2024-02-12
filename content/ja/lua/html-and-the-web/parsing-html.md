---
title:                "HTMLの解析"
aliases: - /ja/lua/parsing-html.md
date:                  2024-02-03T19:12:46.969486-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
HTMLの解析とは、HTML文書からデータや情報を抽出することであり、これはWebスクレイピング、データ分析、自動化タスクに不可欠です。プログラマーはこれを行い、プログラム的にWebコンテンツを収集、分析、または操作し、ウェブサイトからのデータの手動抽出を自動化することができます。

## 方法：
LuaにはHTMLを解析するための組み込みライブラリはありませんが、`LuaHTML`のようなサードパーティのライブラリを利用するか、`LuaXML`を通して`libxml2`のバインディングを利用することができます。一般的なアプローチは、`lua-gumbo`ライブラリを使用してHTMLを解析することであり、これは、直感的で、HTML5に準拠した解析機能を提供します。

### lua-gumboのインストール:
まず、`lua-gumbo`がインストールされていることを確認します。通常、luarocksを使用してインストールできます：

```sh
luarocks install lua-gumbo
```

### lua-gumboを使った基本的な解析:
ここでは、`lua-gumbo`を使用してシンプルなHTMLスニペットを解析し、そこからデータを抽出する方法を示します：

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>Hello, world!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- 出力: Hello, world!
```

### 高度な例 - リンクの抽出:
HTML文書内のすべてのアンカータグ（`<a>`要素）から`href`属性を抽出するには：

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>サンプルページ</title></head>
<body>
  <a href="http://example.com/1">リンク 1</a>
  <a href="http://example.com/2">リンク 2</a>
  <a href="http://example.com/3">リンク 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- エレメントであり、属性を持っていることを確認
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- サンプル出力:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

このコードスニペットは、ドキュメント内のすべてのリンクを反復処理し、それらの`href`属性を印刷します。`lua-gumbo`ライブラリのHTML文書の構造を解析し、理解する能力により、タグや属性に基づいて特定の要素を抽出するプロセスが簡素化されます。
