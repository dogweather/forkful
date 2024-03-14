---
date: 2024-01-20 17:46:01.396536-07:00
description: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u306E\u62BD\u51FA\u3068\u306F\
  \u3001\u5927\u304D\u306A\u6587\u5B57\u5217\u304B\u3089\u5FC5\u8981\u306A\u90E8\u5206\
  \u6587\u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u3092\
  \u884C\u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u51E6\u7406\u3084\u7279\u5B9A\
  \u306E\u6587\u5B57\u60C5\u5831\u306E\u5206\u6790\u306E\u305F\u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.289396-06:00'
model: gpt-4-1106-preview
summary: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u306E\u62BD\u51FA\u3068\u306F\
  \u3001\u5927\u304D\u306A\u6587\u5B57\u5217\u304B\u3089\u5FC5\u8981\u306A\u90E8\u5206\
  \u6587\u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u3092\
  \u884C\u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u51E6\u7406\u3084\u7279\u5B9A\
  \u306E\u6587\u5B57\u60C5\u5831\u306E\u5206\u6790\u306E\u305F\u3081\u3067\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
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
