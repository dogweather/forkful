---
aliases:
- /ja/lua/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:57.412371-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u5168\u3066\u306E\u5927\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u691C\u7D22\u3084\u30BD\u30FC\u30C8\u3092\
  \u7C21\u5358\u306B\u3057\u3001\u5927\u6587\u5B57\u5C0F\u6587\u5B57\u3092\u554F\u308F\
  \u306A\u3044\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u305F\u3081\u306B\u884C\u308F\u308C\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.021939
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u5168\u3066\u306E\u5927\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u691C\u7D22\u3084\u30BD\u30FC\u30C8\u3092\
  \u7C21\u5358\u306B\u3057\u3001\u5927\u6587\u5B57\u5C0F\u6587\u5B57\u3092\u554F\u308F\
  \u306A\u3044\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u305F\u3081\u306B\u884C\u308F\u308C\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を小文字に変換するとは、全ての大文字を小文字にすることです。検索やソートを簡単にし、大文字小文字を問わない一貫性を保つために行われます。

## How to: (方法)
```Lua
-- 文字列を小文字に変換する例
local originalString = "Hello, World!"
local lowerCaseString = originalString:lower()

print(lowerCaseString)  -- 出力: "hello, world!"
```

## Deep Dive (深く探る)
Luaでは、文字列操作は基本的で必要な作業です。`string.lower`関数は、Lua 5.0から利用可能で、大文字から小文字への変換が簡単にできます。この関数は、ASCII文字に限定されており、ローカル言語の特殊な大文字には対応していない場合があります。代替案としては、UTF-8対応の外部ライブラリを使用することが挙げられます。内部的には、Luaの`string.lower`はCで書かれた関数を呼び出して実装されており、実行速度が速いです。

## See Also (関連情報)
- Lua 5.4リファレンスマニュアル: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
