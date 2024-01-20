---
title:                "未来または過去の日付を計算する"
html_title:           "Lua: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何と何故？
未来や過去の日付を計算するとは、ある日から特定の期間を足し引きして新しい日付を導く事を指します。プログラマーがこれを行う理由は、イベントの日程計画、残り時間の計算、年齢の確認、などの機能をアプリケーションに提供するためです。

## 実装方法：
Luaではos.dateとos.time関数を使用して日付の計算を行います。以下に簡単なコードを示します。

```Lua
local now = os.time() -- 現在の日付と時間を取得
print(os.date('%Y-%m-%d', now)) -- 現在の日付を表示

local days = 3*24*60*60 -- 3日後を秒単位で計算
local future_date = now + days -- 未来の日付を計算

print(os.date('%Y-%m-%d', future_date)) -- 未来の日付を表示
```
このコードを実行すると以下のような出力が得られます：
```
2022-04-16
2022-04-19
```
## さらに深く見る：
日付計算はコンピュータ科学の中で歴史的に重要なテーマとなっています。複雑なカレンダーシステムや、グレゴリオ暦とユリウス暦の切り替えによる日付のずれなど、さまざまな課題を解決するために様々なアルゴリズムが開発されてきました。

Luaでは、os.dateとos.time関数以外にも、os.difftime関数を使った日付の計算方法もあります。また、日付と時間を扱うためのライブラリ、たとえばluadateやchronosも存在します。

os.time関数は、1970年1月1日からの経過秒数を返すため、未来や過去の日付を計算する場合はその限界を考慮する必要があります。

## 参考記事：
1. Lua 5.3 リファレンスマニュアル - os.date: [リンク](https://www.lua.org/manual/5.3/manual.html#pdf-os.date)
2. Lua 5.3 リファレンスマニュアル - os.time: [リンク](https://www.lua.org/manual/5.3/manual.html#pdf-os.time)
3. Lua 5.3 リファレンスマニュアル - os.difftime: [リンク](https://www.lua.org/manual/5.3/manual.html#pdf-os.difftime)
4. luadate : [リンク](https://github.com/Tieske/date)