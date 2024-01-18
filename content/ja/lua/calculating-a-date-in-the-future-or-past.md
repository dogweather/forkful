---
title:                "将来または過去の日付の計算"
html_title:           "Lua: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何？なぜ？

未来や過去の日付を計算するとは、特定の日付から一定期間進んだり戻ったりすることを指します。プログラマーがこれを行う理由は、アプリケーションやゲームなどの特定のイベントや機能を起動するために必要な日付を自動的に生成したり、現在の日付から一定期間後の日付を計算する必要がある場合があるためです。

## 使い方：

```
--未来の日付を計算する
local future_date = os.date("%Y/%m/%d", os.time() + 86400)
print(future_date) --出力：明日の日付（YYYY/MM/DDの形式）

--過去の日付を計算する
local past_date = os.date("%Y/%m/%d", os.time() - (86400*7))
print(past_date) --出力：一週間前の日付（YYYY/MM/DDの形式）
```

## 詳細：

1. 歴史的文脈：日付の計算は、プログラムが開発される初期の時点から重要な機能として使用されてきました。特に、コンピューターが普及する前の計算機では、日付の計算を行うプログラムが多数開発されました。

2. 代替手段：Luaには、日付の計算を行うための組み込みのモジュールがありますが、他のプログラミング言語やライブラリも使用することができます。マイクロソフトのVisual Basicなど、ある特定のプログラミング言語には、日付の計算を行うための専用の関数が用意されています。

3. 実装の詳細：未来や過去の日付を計算するためには、Luaのos.time()関数を使用します。この関数は、指定した日付から1970年1月1日までの経過秒数を返します。この値を使って、先ほどのコードのように、未来や過去の日付を計算することができます。

## 関連リンク：

- [Lua - 日付と時刻の操作](https://www.lua.org/manual/5.3/manual.html#6.9)
- [Microsoft - Visual Basicでの日付と時刻の操作](https://docs.microsoft.com/en-us/dotnet/visual-basic/language-reference/functions/date-and-time-functions)