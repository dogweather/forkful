---
date: 2024-01-20 17:33:22.861953-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.577872-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9:) \u65E5\u4ED8\u6BD4\u8F03\u306F\u3001\u30AB\u30EC\u30F3\
  \u30C0\u30FC\u3084\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u30BD\u30D5\u30C8\
  \u30A6\u30A7\u30A2\u306B\u3068\u3063\u3066\u6839\u672C\u7684\u306A\u6A5F\u80FD\u3067\
  \u3059\u3002Lua\u3067\u306F`os.time`\u95A2\u6570\u3092\u4F7F\u3063\u3066\u65E5\u4ED8\
  \u3092UNIX\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\uFF081970\u5E741\u67081\u65E5\
  \u304B\u3089\u306E\u79D2\u6570\uFF09\u306B\u5909\u63DB\u3057\u6BD4\u8F03\u3059\u308B\
  \u306E\u304C\u4E00\u822C\u7684\u3067\u3059\u3002`os.date`\u95A2\u6570\u3082\u65E5\
  \u4ED8\u30C7\u30FC\u30BF\u3092\u6271\u3044\u307E\u3059\u304C\u3001\u6BD4\u8F03\u306E\
  \u305F\u3081\u306B\u306FUNIX\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u304C\u4FBF\
  \u5229\u3067\u3059\u3002\u4ED6\u306E\u8A00\u8A9E\u3067\u306F\u5C02\u7528\u306E\u65E5\
  \u4ED8\u6BD4\u8F03\u6A5F\u80FD\u304C\u3042\u308A\u307E\u3059\u304C\u3001Lua\u306E\
  \u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u6BD4\u8F03\u7684\u5C0F\u3055\u304F\
  \u3001`os`\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u95A2\u6570\u3092\u4F7F\u7528\u3057\
  \u3066\u3053\u306E\u3088\u3046\u306A\u6BD4\u8F03\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## How to: (やり方:)
```Lua
-- 日付の文字列をos.timeで扱える形式に変換する関数
local function parseDate(dateString)
    local pattern = "(%d+)-(%d+)-(%d+)"
    local year, month, day = dateString:match(pattern)
    return os.time({year=year, month=month, day=day})
end

-- 日付を比較する例
local date1 = "2023-03-15"
local date2 = "2023-03-20"

-- 比較の実施
local time1 = parseDate(date1)
local time2 = parseDate(date2)

if time1 > time2 then
    print(date1 .. " is after " .. date2)
elseif time1 < time2 then
    print(date1 .. " is before " .. date2)
else
    print(date1 .. " is the same day as " .. date2)
end
```

出力例:
```
2023-03-15 is before 2023-03-20
```

## Deep Dive (深掘り)
日付比較は、カレンダーやスケジューリングソフトウェアにとって根本的な機能です。Luaでは`os.time`関数を使って日付をUNIXタイムスタンプ（1970年1月1日からの秒数）に変換し比較するのが一般的です。`os.date`関数も日付データを扱いますが、比較のためにはUNIXタイムスタンプが便利です。他の言語では専用の日付比較機能がありますが、Luaの標準ライブラリは比較的小さく、`os`ライブラリの関数を使用してこのような比較を行います。

## See Also (関連項目)
- Lua 5.4 Reference Manual: `os.time`, `os.date` - https://www.lua.org/manual/5.4/manual.html#6.9
- Lua Users Wiki: Date and Time - http://lua-users.org/wiki/DateTime
