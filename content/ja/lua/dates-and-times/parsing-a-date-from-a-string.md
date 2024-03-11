---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:59.757647-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.881137-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
---

{{< edit_this_page >}}

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
