---
date: 2024-01-20 17:58:10.983804-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.547074-06:00'
model: gpt-4-1106-preview
summary: "(\u5B9F\u8DF5\u65B9\u6CD5) Lua\u3067\u306F`string.gsub`\u95A2\u6570\u3092\
  \u4F7F\u3063\u3066\u7C21\u5358\u306B\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\
  \u63DB\u304C\u3067\u304D\u307E\u3059\u30021993\u5E74\u306E\u767B\u5834\u4EE5\u6765\
  \u3001Lua\u306F\u62E1\u5F35\u6027\u3068\u30DD\u30FC\u30BF\u30D3\u30EA\u30C6\u30A3\
  \u304C\u91CD\u8996\u3055\u308C\u3066\u304A\u308A\u3001\u69D8\u3005\u306A\u74B0\u5883\
  \u3067\u4F7F\u308F\u308C\u3066\u3044\u307E\u3059\u3002`gsub`\u306F\u30B0\u30ED\u30FC\
  \u30D0\u30EB\u7F6E\u63DB\u306E\u7565\u3067\u3001\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\
  \u30C1\u30F3\u30B0\u3092\u512A\u308C\u305F\u6A5F\u80FD\u3068\u3057\u3066\u63D0\u4F9B\
  \u3057\u3066\u3044\u307E\u3059\u304C\u3001\u6B63\u898F\u8868\u73FE\u306F\u5B8C\u5168\
  \u306B\u306F\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u3002\u4EE3\
  \u308F\u308A\u306B\u3001Lua\u306E\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\
  \u30B0\u306F\u9650\u5B9A\u7684\u3067\u3059\u304C\u3001\u591A\u304F\u306E\u4E00\u822C\
  \u7684\u306A\u30E6\u30FC\u30B9\u30B1\u30FC\u30B9\u306B\u306F\u5341\u5206\u3067\u3059\
  \u3002\u6B63\u898F\u8868\u73FE\u304C\u5FC5\u8981\u306A\u5834\u5408\u3001\u5916\u90E8\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u30FC\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002"
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
