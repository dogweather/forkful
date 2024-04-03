---
date: 2024-01-20 17:58:10.983804-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u6587\
  \u5B57\u5217\u5185\u3067\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u3092\u898B\u3064\
  \u3051\u3066\u3001\u305D\u308C\u3092\u5225\u306E\u30C6\u30AD\u30B9\u30C8\u3067\u7F6E\
  \u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30C7\u30FC\u30BF\u6574\u7406\u3084\u30B3\u30FC\u30C9\u306E\u4FEE\
  \u6B63\u3067\u3053\u308C\u3092\u3088\u304F\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.283830-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u6587\
  \u5B57\u5217\u5185\u3067\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u3092\u898B\u3064\
  \u3051\u3066\u3001\u305D\u308C\u3092\u5225\u306E\u30C6\u30AD\u30B9\u30C8\u3067\u7F6E\
  \u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30C7\u30FC\u30BF\u6574\u7406\u3084\u30B3\u30FC\u30C9\u306E\u4FEE\
  \u6B63\u3067\u3053\u308C\u3092\u3088\u304F\u4F7F\u3044\u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## How to: (実践方法)
```Lua
local text = "今日は晴れです。明日も晴れるかもしれません。"
local pattern = "晴れ"
local replacement = "雨"

-- 検索して置換
local result = text:gsub(pattern, replacement)

print(result) -- "今日は雨です。明日も雨るかもしれません。"
```

## Deep Dive (深掘り)
Luaでは`string.gsub`関数を使って簡単にテキスト検索と置換ができます。1993年の登場以来、Luaは拡張性とポータビリティが重視されており、様々な環境で使われています。`gsub`はグローバル置換の略で、パターンマッチングを優れた機能として提供していますが、正規表現は完全にはサポートしていません。代わりに、Luaのパターンマッチングは限定的ですが、多くの一般的なユースケースには十分です。正規表現が必要な場合、外部ライブラリーを利用することができます。

## See Also (関連情報)
- Lua 5.4 Reference Manual: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
