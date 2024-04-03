---
date: 2024-01-20 17:51:31.664027-07:00
description: "How to (\u3084\u308A\u65B9): ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.285208-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
