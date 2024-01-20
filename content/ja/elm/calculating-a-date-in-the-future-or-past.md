---
title:                "未来または過去の日付の計算"
html_title:           "Elm: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 過去や未来の日付を計算する、「なぜ」と「何の？」

過去や未来の日付を計算するとは、単にある特定の日付に何日、何週間、または何年を足したり引いたりすることです。プログラマーはこれをシステムが予定、リマインダー、スケジューリングタスクを処理できるようにするために行います。

# どうやって：

Elmで過去や未来の日付を計算するための例を以下に示します。

```Elm
import Time.Extra exposing (..)
import Time exposing (..)

main =
    Task.perform Time.here
        (\time ->
            let
                (_, nextWeek) =
                    add (week 1) time
                        |> toTime
                        |> Date.fromTime
                        |> Maybe.withDefault (Date (Date.year 2021) Month.Jan 1)
                        |> Date.toCalendarDate
            in
            nextWeek
                |> toString
                |> Debug.log "A week from now will be"
        )
```

# ディープダイブ：

1. 歴史的な視点から,既存のパッケージで計算を行う前に、自己実装をしなければならない日がありました。しかし、現在ではElmのTime.Extraライブラリなど、日付から一定の間隔を加えるための関数を提供する多くのパッケージがあります。

2. Elmの他の日付操作ライブラリの代替としては、`justinmimbs/date-extra`があります。これは、さまざまな日付形式と難しいテストケースを処理する際に役立つ一連のユーティリティを提供します。

3. 日付を未来や過去に計算する実装の詳細を探ると、まず、現在の日付と時間が取得され、それに特定の間隔が追加されます。その結果は新しい日付オブジェクトとなり、それが最終的に文字列に変換されて出力されます。

# 参考資料：

以下に関連するリソースへのリンクを示します:
- Elmの公式ドキュメンテーション（日付と時間）: [リンク](https://package.elm-lang.org/packages/elm/time/latest/)
- GitHub上の`justinmimbs/date-extra`パッケージ : [リンク](https://github.com/justinmimbs/date-extra)
- Elmの日付や時間に関するチュートリアル: [リンク](https://elmprogramming.com/datetime.html)