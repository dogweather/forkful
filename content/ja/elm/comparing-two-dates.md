---
title:                "日付を比較する"
date:                  2024-01-20T17:33:14.408444-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を比較する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
比較って何？それはプログラムにおいて、二つの日付がどう関連してるかを確かめることだ。なぜやるのか？イベントがいつ起こるのかをスケジュールしたり、期限を監視したりするためさ。

## How to:
Elmで日付を比較する方法を見てみよう。ちゃちゃっとコード書いて、実際に動かす所までやってみよう。

```Elm
import Time exposing (Posix)
import Task
import Date exposing (Date)

-- 日付を比較する関数
compareDates : Date -> Date -> Order
compareDates date1 date2 =
    Date.compare date1 date2

-- 実用例を見てみよう
example : Task.Task x String
example =
    Task.map (\o ->
        case o of
            LT ->
                "第一の日付の方が古いよ"

            EQ ->
                "二つの日付は同じだね"

            GT ->
                "第一の日付の方が新しいね"
    )
    (Task.map2 compareDates (Date.fromPosix (Posix 0)) (Date.fromPosix (Posix 86400000)))
```

サンプル出力の ```
"第一の日付の方が新しいね"
``` というのは、第一の日付が第二の日付より一日新しい場合の結果だね。

## Deep Dive
Elmの日付比較は、JavaScriptより簡潔でエラーが少ない。`Date` モジュールはElmの初期バージョンからあるが、今ではより安全で使いやすくなっている。他の方法としては、日付をUnixタイムスタンプ（`Posix`）に変換して比較することもできる。実装の詳細については、Elmの内部で`compare`関数がどう動いてるかを理解することが大事だ。それぞれの日付を内部的な数値として見て、その数値を比較するんだ。

## See Also
- [Elm Date Documentation](https://package.elm-lang.org/packages/justinmimbs/date/latest/): `Date` モジュールの公式ドキュメンテーション
- [Elm Time Documentation](https://package.elm-lang.org/packages/elm/time/latest/): `Time` モジュールに関する詳細
- [Elm Discuss](https://discourse.elm-lang.org/): Elmコミュニティでの議論ができるフォーラム