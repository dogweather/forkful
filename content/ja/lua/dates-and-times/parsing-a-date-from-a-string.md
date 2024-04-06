---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:59.757647-07:00
description: "\u65B9\u6CD5\uFF1A Lua\u306F\u3001`os.date`\u95A2\u6570\u3068`os.time`\u95A2\
  \u6570\u306B\u3088\u3063\u3066\u63D0\u4F9B\u3055\u308C\u308B\u9650\u5B9A\u7684\u306A\
  \u6A5F\u80FD\u3092\u9664\u3044\u3066\u3001\u65E5\u4ED8\u3068\u6642\u523B\u306E\u64CD\
  \u4F5C\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u3002\u3057\
  \u304B\u3057\u3001\u3053\u308C\u3089\u306F\u57FA\u672C\u7684\u306A\u89E3\u6790\u306B\
  \u5229\u7528\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u3001\u3088\u308A\u8907\u96D1\
  \u306A\u8981\u4EF6\u306B\u5BFE\u3057\u3066\u306F\u3001\u5916\u90E8\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3067\u3042\u308B`luadate`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5229\
  \u7528\u3067\u304D\u307E\u3059\u3002 **`os.date`\u2026"
lastmod: '2024-04-05T21:53:43.162618-06:00'
model: gpt-4-0125-preview
summary: "**`os.date` \u3068 `os.time`\u3092\u4F7F\u3046\uFF1A**."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## 方法：
Luaは、`os.date`関数と`os.time`関数によって提供される限定的な機能を除いて、日付と時刻の操作をサポートしていません。しかし、これらは基本的な解析に利用することができ、より複雑な要件に対しては、外部ライブラリである`luadate`ライブラリを利用できます。

**`os.date` と `os.time`を使う：**
```lua
-- 人間が読める日付をタイムスタンプに変換し、それを元に戻す
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)

local timestamp = os.time({
  year = year,
  month = month,
  day = day,
  hour = hour,
  min = minute,
  sec = second
})

-- タイムスタンプを人間が読める形式に変換する
local formattedDate = os.date("%Y-%m-%d %H:%M:%S", timestamp)
print(formattedDate)  -- 出力: 2023-09-21 15:00:00
```

**`luadate`（サードパーティライブラリ）を使用する：**
`luadate`を使用するには、LuaRocksやお好みのパッケージマネージャー経由でインストールされていることを確認してください。`luadate`は、日付と時刻の解析と操作の機能を広範囲に提供します。

```lua
local date = require('date')

-- 日付文字列を直接解析する
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- 出力: 2023-09-21 15:00:00

-- 期間を追加する
local oneWeekLater = parsedDate:adddays(7)
print(oneWeekLater:fmt("%Y-%m-%d %H:%M:%S"))  -- 出力: 2023-09-28 15:00:00
```

`luadate`ライブラリは、文字列からの解析、フォーマット、日付の算術操作を含む、日付に関するより直感的で強力な方法を提供します。これにより、Luaでの時間データの取り扱いがかなり簡素化されます。
