---
date: 2024-01-20 17:46:01.396536-07:00
description: "How to: (\u65B9\u6CD5) Lua\u3067\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\
  \u30B0\u3092\u62BD\u51FA\u3059\u308B\u306B\u306F\u6A19\u6E96\u95A2\u6570 `string.sub`\
  \ \u3092\u4F7F\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u4F7F\u3044\u65B9\u3067\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.289396-06:00'
model: gpt-4-1106-preview
summary: "Lua\u3067\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u62BD\u51FA\u3059\
  \u308B\u306B\u306F\u6A19\u6E96\u95A2\u6570 `string.sub` \u3092\u4F7F\u3044\u307E\
  \u3059\u3002\u4EE5\u4E0B\u306F\u4F7F\u3044\u65B9\u3067\u3059."
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

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
