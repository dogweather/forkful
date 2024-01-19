---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Elmでの日付を文字列に変換する方法

## 何となぜ?

日付を文字列に変換するとは、日付データを人間が読み理解しやすいテキスト形式にすることです。これは、ユーザーに日付情報を表示したり、日付データを文字列ベースのデータベースに保存したりするために行われます。

## 実装方法

```Elm
import Time
import Time.Extra

currentDateToString : Time.Posix -> String
currentDateToString time =
    let
        zone = Time.here
    in
    Time.toAdjustedZone zone time
        |> Time.Extra.formatISO8601Millis

main =
    Time.now
        |> Task.perform currentDateToString
        |> Html.program String.empty
```

このコード例では `Time` と `Time.Extra` モジュールを利用しています。現在時間を取得し、それをISO8601形式の文字列に変換しています。

## Deep Dive

日付の文字列への変換は歴史的にあらゆるプログラミング言語で実装されてきました。これは主にデータベースとの互換性や、人間が読める形式への変換のニーズによるものです。

Elmにおける日付の文字列への変換の実装は `Time` モジュールに組み込まれており、さらに汎用的な変換関数は `Time.Extra` モジュールにある。

日付から文字列への変換は、あくまで一つの解決策であり、要件により異なる解決策が必要となります。そのため、自身のプロジェクトの要件に応じて最適な方法を選ぶことが重要です。

## See Also

- Elm公式ドキュメンテーションの[Time](https://package.elm-lang.org/packages/elm/time/latest)モジュール
- [Time.Extra](https://package.elm-lang.org/packages/justinmimbs/time-extra/latest)モジュールのドキュメンテーション
- ISO8601形式についての[解説記事](https://www.cl.cam.ac.uk/~mgk25/iso-time.html)