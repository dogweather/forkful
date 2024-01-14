---
title:                "Elm: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

日付を取得する理由は何でしょうか？ Elmプログラミングをする上で、現在の日付を取得する必要がある場合があります。たとえば、ブログ記事の投稿日を表示したり、期限を設定するアプリケーションを作成する際には、現在の日付を取得する必要があります。

## 方法

現在の日付を取得するためには、 `Date` モジュールを使用します。まず、 `Date.now` 関数を使って現在のタイムスタンプを取得します。次に、 `Date.fromTime` 関数を使ってタイムスタンプを日付に変換します。

```Elm
import Date

timestamp = Date.now
currentDate = Date.fromTime timestamp
```

上記のコードを実行すると、 `currentDate` の値は以下のようになります。

```Elm
"2021-07-16"
```

日付フォーマットをカスタマイズしたい場合は、`Date.Format` モジュールを使用することもできます。例えば、以下のように日付のフォーマットを指定することができます。

```Elm
import Date
import Date.Format exposing (custom)

timestamp = Date.now
formattedDate = Date.Format.custom "YYYY.MM.dd" timestamp
```

上記のコードを実行すると、 `formattedDate` の値は以下のようになります。

```Elm
"2021.07.16"
```

## ディープダイブ

日付を取得する際には、タイムゾーンの考慮も重要です。 `Date` モジュールを使用する場合は、タイムゾーンに関する設定を行うことができます。また、 `Date.fromTimezone` 関数を使って、指定したタイムゾーンを考慮した日付を取得することもできます。

```Elm
import Date
import Date.Timezone exposing (timezone)

japanTimezone =  timezone 9 0
timestamp = Date.now
japanDate = Date.fromTimezone japanTimezone timestamp
```

## 参考リンク

- [Elm Dateモジュールドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm Date.Timezoneモジュールドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Date-Timezone)
- [Elm Date.Formatモジュールドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Date-Format)
- [TsuruokaDai/elm-jstz](https://github.com/TsuruokaDai/elm-jstz)