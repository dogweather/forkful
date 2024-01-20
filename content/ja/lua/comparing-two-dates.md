---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## どういうこと？なぜ？

日付を比較するとは、二つの日付が同じか、またはその一方が他方よりも前もしくは後であるかを確認するプロセスです。プログラマは、イベントの順序を決定したり、日付に基づく決定を行うためにこの手法を使います。

## どうやって：

Luaには日付の操作をサポートする多くのライブラリがありますが、ここではosライブラリを使います。

```Lua
local t1 = os.time({year=2023, month=7, day=10})
local t2 = os.time({year=2023, month=7, day=15})

if t1 < t2 then
  print("July 10, 2023 is before July 15, 2023")
else
  print("July 10, 2023 is not before July 15, 2023")
end
```

出力結果は、
```
July 10, 2023 is before July 15, 2023
```

## 詳解

### 歴史 

Lua语言は1993年にリリースされ、日付の比較をするための組み込み関数がありませんでした。しかし、時間経過とともに、osライブラリのos.time関数が使われるようになりました。

### 代替法

日付比較を別の方法で行いたい場合は、Luaで使用できる日付操作をサポートする外部ライブラリを用いることができます。例えば、luadateやlua-tzライブラリなどがいます。

### 実装詳細

os.time関数は、指定した日付をUNIXエポック（1970年1月1日0時0分0秒からの秒数）に変換します。この関数により、簡単に二つの日付を比較し、一つが他方より前または後であるかを確認できます。

## 参照資料

[公式Lua 5.4 マニュアル - osライブラリ](http://www.lua.org/manual/5.4/manual.html#6.9)

[日付操作ライブラリ LuaDate](https://github.com/Tieske/date)

[Luaで時間を扱うライブラリ lua-tz](https://github.com/daurnimator/lua-tz)