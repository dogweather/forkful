---
title:                "「現在の日付を取得する」"
html_title:           "Lua: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何が & なぜ?

「現在の日付を取得する」とは、プログラマーがコンピューターの現在の日付を取得することを意味します。このような機能を必要とする理由は、日付に関連するタスクを実行するために、プログラム上で日付を使用する必要があるからです。

## 方法:

```Lua
-- 現在の日付を取得する
local currentDate = os.date()
 
-- オプションを指定してフォーマットを変更する
local customDate = os.date("%Y年%m月%d日", os.time())
 
-- 曜日の情報を取得する
local dayOfWeek = os.date("%A", os.time())
```

```Lua
-- サンプル出力

currentDate:  2021-10-07 14:00:00
customDate:  2021年10月07日
dayOfWeek:  Thursday
```

## 深く掘り下げる:

- プログラミング言語Luaでは、日付や時間を取得するための組み込み関数が用意されています。これらの関数を使用することで、必要に応じて日付や時間のフォーマットをカスタマイズすることができます。
- 別の方法として、外部ライブラリを使用することもできます。たとえば、Luaの拡張機能であるLua Powerを使うと、より多くの日付や時間の処理を行うことができます。
- 日付と時間は、コンピューターやソフトウェアの動作において非常に重要な役割を担っています。すべてのシステムやアプリケーションは、現在の日付や時刻を正しく判断する必要があります。

## さらに参考に:

- [Lua Language Reference](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Lua Power](https://github.com/xavier-wang/lue-power)