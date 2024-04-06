---
date: 2024-01-20 17:35:24.074177-07:00
description: "How to: (\u3084\u308A\u65B9) \u6587\u5B57\u5217\u306E\u9023\u7D50\u306F\
  Lua\u306E\u6B74\u53F2\u3092\u901A\u3058\u3066\u57FA\u672C\u7684\u306A\u6A5F\u80FD\
  \u3067\u3042\u308A\u3001Lua\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.819910-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6587\u5B57\u5217\u306E\u9023\u7D50\u306FLua\u306E\
  \u6B74\u53F2\u3092\u901A\u3058\u3066\u57FA\u672C\u7684\u306A\u6A5F\u80FD\u3067\u3042\
  \u308A\u3001Lua 5.1\u304B\u3089\u306F`..`\u6F14\u7B97\u5B50\u3092\u4F7F\u3063\u3066\
  \u7C21\u5358\u306B\u884C\u308F\u308C\u307E\u3059\u3002\u6587\u5B57\u5217\u306F\u5185\
  \u90E8\u7684\u306B\u306F\u4E0D\u5909\u3067\u3042\u308A\u3001\u65B0\u3057\u3044\u6587\
  \u5B57\u5217\u3092\u4F5C\u308B\u305F\u3073\u306B\u30E1\u30E2\u30EA\u306B\u65B0\u3057\
  \u3044\u9818\u57DF\u304C\u5272\u308A\u5F53\u3066\u3089\u308C\u308B\u305F\u3081\u3001\
  \u5927\u91CF\u306E\u9023\u7D50\u64CD\u4F5C\u306F\u6027\u80FD\u306B\u5F71\u97FF\u3092\
  \u4E0E\u3048\u308B\u53EF\u80FD\u6027\u304C\u3042\u308A\u307E\u3059\u3002\u305D\u306E\
  \u305F\u3081\u3001`table.concat`\u95A2\u6570\u306F\u591A\u6570\u306E\u6587\u5B57\
  \u5217\u3092\u9023\u7D50\u3059\u308B\u969B\u306E\u52B9\u7387\u7684\u306A\u9078\u629E\
  \u80A2\u3068\u306A\u308A\u3048\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## How to: (やり方)
```Lua
-- 文字列を連結する基本的な方法
local greeting = "こんにちは"
local name = "世界"
local message = greeting .. ", " .. name .. "!"
print(message)  -- 出力: こんにちは, 世界!
```

```Lua
-- テーブルを使って効果的に連結
local words = {"Luaは", "素晴らしい", "言語です"}
local sentence = table.concat(words, " ") -- スペースをセパレータとして使用
print(sentence)  -- 出力: Luaは 素晴らしい 言語です
```

## Deep Dive (掘り下げ)
文字列の連結はLuaの歴史を通じて基本的な機能であり、Lua 5.1からは`..`演算子を使って簡単に行われます。文字列は内部的には不変であり、新しい文字列を作るたびにメモリに新しい領域が割り当てられるため、大量の連結操作は性能に影響を与える可能性があります。そのため、`table.concat`関数は多数の文字列を連結する際の効率的な選択肢となりえます。

Luaはメタメソッド`__concat`もサポートしており、カスタムオブジェクト間の連結を定義するのに使われます。連結操作の裏で実際に行われるプロセスはLuaバージョンによって若干異なるため、Luaのドキュメントを確認することを推奨します。

## See Also (関連する情報)
- [Lua 5.4リファレンスマニュアル: 文字列](https://www.lua.org/manual/5.4/manual.html#3.4.6)
