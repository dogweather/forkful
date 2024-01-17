---
title:                "日付を文字列に変換する"
html_title:           "Elm: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
日付を文字列に変換することは、プログラマーにとって非常に重要な作業です。これは、日付をデータとして扱う必要がある場合や、ユーザーにわかりやすい形式で日付を表示する必要がある場合に役立ちます。

## 方法：
日付を文字列に変換するためには、Elmの組み込み関数である`Format`モジュールを使用します。`Format.date`関数を使用することで、任意の書式で日付を文字列に変換することができます。例えば、以下のように記述することで、現在の日付を年-月-日の形式で表示することができます。

```Elm
Elm.Date.Format.date "%Y-%m-%d" (Elm.Date.now)
-- 例: "2021-10-30"
```

## 深く掘り下げる：
日付を文字列に変換する方法には、他にもいくつかのオプションがあります。例えば、Elmの`Time`モジュールを使用することで、タイムゾーンやロケールを考慮したより高度な日付のフォーマットが可能です。また、`Date.toUtcString`関数を使用することで、日付を国際標準のISO 8601形式に変換することもできます。

## 関連情報：
- Elm公式ドキュメンテーション(https://guide.elm-lang.jp/dates.html)
- ElmのFormatモジュールのドキュメンテーション(https://package.elm-lang.org/packages/elm/core/latest/Date-Format)
- ElmのTimeモジュールのドキュメンテーション(https://package.elm-lang.org/packages/elm/time/latest/Time-Format)