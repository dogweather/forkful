---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？

現在の日付の取得は、その瞬間の年、月、日をプログラムに取り込むことを意味します。これは、ログファイルのタイムスタンプ付けやユーザーの行動のタイムスタンプ付けなど、時間に敏感なデータを扱うときによく使われます。

## どういう手順で？

Luaで現在の日付を取得する一番シンプルな方法を見ていきましょう。

```Lua
-- require the os library
os = require("os")

-- Get the current date and time
print(os.date())
```

このコードは、現在の日付と時刻を次のように表示します：

```Lua
"Tue Feb 4 23:48:10 2022"
```
これは一般的な日付の表記方ですが、特定の形式で日付を取得することも可能です：

```Lua
-- example for specific date format
print(os.date("%A, %B %d, %Y"))
```
これで、次のような出力が得られます：

```Lua
"Tuesday, February 4, 2022"
```

## 深掘り

Luaのos.date関数は、C言語の標準ライブラリの便利な関数を利用しています。`os.date`は、時間を組み立てるための形式文字列を取り、現在のシステム時間を取得するか、指定した時間を取得します。

また、より高度な日付と時間の操作を行いたい場合は、LuaRocksを通じて利用できるLuaの日付と時間のライブラリ、たとえば、`lua-date` などを検討することもできます。

## 参考資料

以下は、Luaで日付を扱うためのさらに詳しい情報を見つけるためのリンク先です。

- Luaの公式ドキュメンテーション: [os.date](https://www.lua.org/manual/5.2/manual.html#pdf-os.date)
- 総合的なチュートリアル: [Lua Date and Time](https://www.tutorialspoint.com/lua/lua_date_and_time.htm)
- lua-dateライブラリ公式ドキュメンテーション: [lua-date](https://github.com/Tieske/date)