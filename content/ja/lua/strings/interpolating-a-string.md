---
date: 2024-01-20 17:51:31.664027-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u306B\
  \u5909\u6570\u306E\u5024\u3084\u5F0F\u306E\u7D50\u679C\u3092\u57CB\u3081\u8FBC\u3080\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\
  \u52D5\u7684\u306B\u5185\u5BB9\u3092\u5909\u3048\u305F\u3044\u5834\u5408\u3084\u3001\
  \u30B3\u30FC\u30C9\u3092\u7C21\u6F54\u306B\u4FDD\u3061\u305F\u3044\u3068\u304D\u306B\
  \u4FBF\u5229\u3060\u304B\u3089\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.849202-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u306B\
  \u5909\u6570\u306E\u5024\u3084\u5F0F\u306E\u7D50\u679C\u3092\u57CB\u3081\u8FBC\u3080\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\
  \u52D5\u7684\u306B\u5185\u5BB9\u3092\u5909\u3048\u305F\u3044\u5834\u5408\u3084\u3001\
  \u30B3\u30FC\u30C9\u3092\u7C21\u6F54\u306B\u4FDD\u3061\u305F\u3044\u3068\u304D\u306B\
  \u4FBF\u5229\u3060\u304B\u3089\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
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
