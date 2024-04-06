---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:46.969486-07:00
description: "\u65B9\u6CD5\uFF1A Lua\u306B\u306FHTML\u3092\u89E3\u6790\u3059\u308B\
  \u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u3042\
  \u308A\u307E\u305B\u3093\u304C\u3001`LuaHTML`\u306E\u3088\u3046\u306A\u30B5\u30FC\
  \u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5229\u7528\
  \u3059\u308B\u304B\u3001`LuaXML`\u3092\u901A\u3057\u3066`libxml2`\u306E\u30D0\u30A4\
  \u30F3\u30C7\u30A3\u30F3\u30B0\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002\u4E00\u822C\u7684\u306A\u30A2\u30D7\u30ED\u30FC\u30C1\u306F\
  \u3001`lua-\u2026"
lastmod: '2024-04-05T21:53:43.148497-06:00'
model: gpt-4-0125-preview
summary: ''
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
