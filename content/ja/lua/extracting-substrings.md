---
title:                "部分文字列の抽出"
date:                  2024-01-20T17:46:01.396536-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
サブストリングの抽出とは、大きな文字列から必要な部分文を取り出すことです。これを行う理由は、データ処理や特定の文字情報の分析のためです。

## How to: (方法)
Luaでサブストリングを抽出するには標準関数 `string.sub` を使います。以下は使い方です。

```Lua
local text = "こんにちは、世界！"
local substring = string.sub(text, 7, 9)
print(substring) -- 世界
```

サンプル出力:
```
世界
```

もっと簡単に取り出すには、始点だけを指定しても良いです。

```Lua
local substring = string.sub(text, 7)
print(substring) -- 世界！
```

## Deep Dive (深堀り)
Luaの`string.sub`関数は、バージョン5.1から利用できます。他のプログラミング言語では異なる関数やメソッドを使うこともあるので、その点を覚えておきましょう。実装の詳細として、`string.sub`は1から始まるインデックスを使い、指定した範囲までの部分文字列を返します。始点または終点を省略すると、始点は文字列の始まり、終点は文字列の終わりと解釈されます。

## See Also (関連項目)
- Lua マニュアル: [string.sub](https://www.lua.org/manual/5.4/manual.html#pdf-string.sub)
- LuaユーザーズWiki: [StringsTutorial](http://lua-users.org/wiki/StringsTutorial)