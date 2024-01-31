---
title:                "将来または過去の日付を計算する"
date:                  2024-01-20T17:31:11.728636-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"

category:             "Elm"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
## なにを、どうして？
日付の計算は将来または過去の特定の日時を求めることです。プログラマーは、期限の管理、予約システム、履歴追跡などのためにこれを行います。

## How to:
## どうやって：
Elmで日付を計算するサンプルコードです。以下のコードを使用して、未来や過去の日付をどのように計算するかを示します。

```Elm
import Time
import Task
import Date exposing (Date)

calculateFutureDate : Date -> Int -> Task.Task Time.Error Date
calculateFutureDate baseDate daysToAdd =
    Date.toTime baseDate
        |> Task.andThen (\baseTime ->
            Task.succeed (Time.millisToPosix (Time.posixToMillis baseTime + daysToAdd * 86400000))
        )
        |> Task.andThen Date.fromTime

calculatePastDate : Date -> Int -> Task.Task Time.Error Date
calculatePastDate baseDate daysToSubtract =
    calculateFutureDate baseDate (negate daysToSubtract)

main =
    let
        today = Date.fromTime (Time.millisToPosix 1637954400000) -- Example: Nov 27, 2021
    in
    Task.perform Debug.log (calculateFutureDate today 10)
    -- Sample Output: Dec 07, 2021
    
    Task.perform Debug.log (calculatePastDate today 10)
    -- Sample Output: Nov 17, 2021
```

## Deep Dive:
## 掘り下げ：
日付の計算機能はElmの標準ライブラリでは直接サポートされていません。そのため、`Date`モジュールをうまく活用して時間をミリ秒単位で計算します。過去には、よりシンプルな日付処理ライブラリがあったものの、現在は`elm/time`パッケージが中心です。

代替として、カスタムの日付処理関数を書くこともできますし、第三者のパッケージを利用することもできます。しかし、ElmではPure FunctionとImmutabilityが重要なため、副作用があるDate操作が制限されています。

計算の際には、ミリ秒単位での加算や減算を行い、それを`Date`型に戻す必要があります。日付計算は慎重に行う必要があるため、Time Zoneやうるう年を考慮することが大切です。

## See Also:
## 関連リンク：
- [Elm Time package](https://package.elm-lang.org/packages/elm/time/latest/)
- [Moment.js for JavaScript date manipulation](https://momentjs.com/)
- [Elm discourse for community discussions](https://discourse.elm-lang.org/)
