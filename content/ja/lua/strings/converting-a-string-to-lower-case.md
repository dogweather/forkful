---
date: 2024-01-20 17:38:57.412371-07:00
description: "How to: (\u65B9\u6CD5) Lua\u3067\u306F\u3001\u6587\u5B57\u5217\u64CD\
  \u4F5C\u306F\u57FA\u672C\u7684\u3067\u5FC5\u8981\u306A\u4F5C\u696D\u3067\u3059\u3002\
  `string.lower`\u95A2\u6570\u306F\u3001Lua\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.135542-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Lua\u3067\u306F\u3001\u6587\u5B57\u5217\u64CD\u4F5C\u306F\
  \u57FA\u672C\u7684\u3067\u5FC5\u8981\u306A\u4F5C\u696D\u3067\u3059\u3002`string.lower`\u95A2\
  \u6570\u306F\u3001Lua 5.0\u304B\u3089\u5229\u7528\u53EF\u80FD\u3067\u3001\u5927\u6587\
  \u5B57\u304B\u3089\u5C0F\u6587\u5B57\u3078\u306E\u5909\u63DB\u304C\u7C21\u5358\u306B\
  \u3067\u304D\u307E\u3059\u3002\u3053\u306E\u95A2\u6570\u306F\u3001ASCII\u6587\u5B57\
  \u306B\u9650\u5B9A\u3055\u308C\u3066\u304A\u308A\u3001\u30ED\u30FC\u30AB\u30EB\u8A00\
  \u8A9E\u306E\u7279\u6B8A\u306A\u5927\u6587\u5B57\u306B\u306F\u5BFE\u5FDC\u3057\u3066\
  \u3044\u306A\u3044\u5834\u5408\u304C\u3042\u308A\u307E\u3059\u3002\u4EE3\u66FF\u6848\
  \u3068\u3057\u3066\u306F\u3001UTF-8\u5BFE\u5FDC\u306E\u5916\u90E8\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u304C\u6319\u3052\u3089\u308C\
  \u307E\u3059\u3002\u5185\u90E8\u7684\u306B\u306F\u3001Lua\u306E`string.lower`\u306F\
  C\u3067\u66F8\u304B\u308C\u305F\u95A2\u6570\u3092\u547C\u3073\u51FA\u3057\u3066\u5B9F\
  \u88C5\u3055\u308C\u3066\u304A\u308A\u3001\u5B9F\u884C\u901F\u5EA6\u304C\u901F\u3044\
  \u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
