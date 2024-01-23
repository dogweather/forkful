---
title:                "文字列の補間"
date:                  2024-01-20T17:51:31.664027-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

文字列補間とは、文字列に変数の値や式の結果を埋め込むことです。これを行う理由は、動的に内容を変えたい場合や、コードを簡潔に保ちたいときに便利だからです。

## How to (やり方):

```Lua
-- 基本的な文字列補間の例
local name = "世界"
local greeting = ("こんにちは、%s！"):format(name)
print(greeting)  -- 出力: こんにちは、世界！

-- 複数の変数を含む場合
local temperature = 23
local message = ("現在の気温は%d度です。"):format(temperature)
print(message)  -- 出力: 現在の気温は23度です。
```

## Deep Dive (深堀り):

文字列補間は、Luaでは`string.format()`関数を使って行います。この関数はC言語の`printf`関数の影響を受けており、それに加えLuaの表現力を活かしています。

代替として文字列の連結がありますが、これは読みにくく、エラーが発生しやすいため、補間が推奨されます。

実装の詳細では、`string.format()`は内部でフォーマット指定子を使い、変数を適切な文字列に変換します。例えば`%s`は文字列、`%d`は整数です。Lua 5.3からは、`utf8`ライブラリが導入され、文字列操作の幅が広がりました。

## See Also (関連情報):

- [Lua 5.4 Reference Manual (文字列)](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Programming in Lua (文字列操作)](https://www.lua.org/pil/20.1.html)
- [Lua-users Wiki (文字列補間)](http://lua-users.org/wiki/StringInterpolation)
