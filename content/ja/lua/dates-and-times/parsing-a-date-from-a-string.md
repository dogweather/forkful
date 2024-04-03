---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:59.757647-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.322705-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\u65E5\u4ED8\u3084\u6642\u523B\u306E\u30C6\u30AD\u30B9\u30C8\u8868\
  \u73FE\u3092\u3001Lua\u30D7\u30ED\u30B0\u30E9\u30E0\u5185\u3067\u7C21\u5358\u306B\
  \u64CD\u4F5C\u3057\u305F\u308A\u3001\u683C\u7D0D\u3057\u305F\u308A\u3001\u6BD4\u8F03\
  \u3057\u305F\u308A\u3067\u304D\u308B\u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\u30ED\u30B0\u8A18\
  \u9332\u3001\u307E\u305F\u306F\u4EFB\u610F\u306E\u6642\u9593\u8A08\u7B97\u3092\u5BB9\
  \u6613\u306B\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u4EBA\u9593\u304C\u8AAD\
  \u3081\u308B\u65E5\u4ED8\u5F62\u5F0F\u3068\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u304C\
  \u52B9\u7387\u7684\u306B\u51E6\u7406\u3067\u304D\u308B\u69CB\u9020\u5316\u3055\u308C\
  \u305F\u30C7\u30FC\u30BF\u578B\u3068\u306E\u9593\u306E\u30AE\u30E3\u30C3\u30D7\u3092\
  \u57CB\u3081\u308B\u305F\u3081\u306B\u3001\u3053\u306E\u30BF\u30B9\u30AF\u3092\u5B9F\
  \u884C\u3057\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## はじめに：何となぜ？
文字列から日付を解析するとは、日付や時刻のテキスト表現を、Luaプログラム内で簡単に操作したり、格納したり、比較したりできる形式に変換することを意味します。プログラマーは、スケジューリング、ログ記録、または任意の時間計算を容易にするため、または人間が読める日付形式とコンピュータが効率的に処理できる構造化されたデータ型との間のギャップを埋めるために、このタスクを実行します。

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
