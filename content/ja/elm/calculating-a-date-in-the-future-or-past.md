---
title:                "Elm: 「未来または過去の日付を計算する」"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
私たちの日常生活の中で、未来や過去の日付を計算する必要があります。たとえば、誕生日を計算したり、トレーニングプログラムを立てたりする際に、将来の日付を計算する必要があります。そのため、Elmで日付を計算する必要が生じることがあります。

## 方法
まず、Dateパッケージをインストールする必要があります。それから、以下の例のようなコードを書きます。例では、現在の日付から1ヶ月後の日付を計算し、出力します。

```Elm
import Date exposing (..)

today = Date.now
nextMonth = Date.add 1 Month today

main = 
    Html.text (toString nextMonth)
```

出力は以下のようになります。

```
2019-07-15T00:00:00.000+09:00
```

さらに、過去の日付を計算することもできます。例えば、現在の日付から1年前の日付を計算するには、以下のようなコードを書きます。

```Elm
import Date exposing (..)

today = Date.now
lastYear = Date.sub 1 Year today

main = 
    Html.text (toString lastYear)
```

出力は以下のようになります。

```
2018-07-15T00:00:00.000+09:00
```

## 深堀り
日付を計算するには、Dateパッケージの他にもいくつかのパッケージが利用できます。例えば、Timeパッケージを使うと、時間やタイムゾーンを考慮した計算ができます。また、Calendarパッケージを使うと、曜日や月の長さなどの情報を取得できます。さらに、Intlパッケージを使うと、地域に応じたローカライズされた日付を取得できます。これらのパッケージを組み合わせることで、より複雑な日付計算が可能になります。

## 関連リンク
- [Dateパッケージのドキュメント](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [Timeパッケージのドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Calendarパッケージのドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Calendar)
- [Intlパッケージのドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Intl)