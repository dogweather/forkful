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

## なぜ計算するのか

日付を計算する理由はさまざまです。例えば、特定のイベントまでの日数を計算したり、期限に間に合うかどうかを確認したり、プロジェクトの進捗を把握するためにも便利です。計算することで、より具体的な目標に向かって効率的に仕事を進めることができます。

## 方法

```Elm
import Time
import Date exposing (..)

-- 今日の日付を取得
now = Date.today

-- 3日後の日付を計算
futureDate = Date.add (Time.inDays 3) now

-- 10日前の日付を計算
pastDate = Date.sub (Time.inDays 10) now

-- 計算結果をコンソールに表示
Future Date: Aug 20, 2021
Past Date: Aug 7, 2021

```

上記のコードでは、`Time`モジュールを使用して日付の差を計算し、`Date`モジュールを使用してその差を現在の日付に適用します。また、日付の表示もより読みやすくするために`Date.today`関数を使用しています。このように、Elmを使用することで、日付の計算と表示を簡単に行うことができます。

## ディープダイブ

Elmは静的型付け言語であり、タイプミスやデータ型の不一致を事前に検出することで、バグの発生を防ぐことができます。また、Elmコードは再利用性が高く、コード量も少なくて済むため、日付計算にも最適です。

## 参考リンク

- [Elm Official Website](https://elm-lang.org/)
- [Elm Time Module Documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Date Module Documentation](https://package.elm-lang.org/packages/elm/date/latest/)