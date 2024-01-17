---
title:                "「現在の日付を取得する」"
html_title:           "Haskell: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## [何を？ & なぜ？]

現在の日付を取得するとは、貴方が今そのコンピュータ上で何日目にいるか知ることです！プログラマーにとっては、コードをより優雅に、そしてより有用にするために日付を取得することが重要です。

## [方法：]

```Haskell
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Locale

getCurrentDate :: IO (Integer, Int, Int)
getCurrentDate = do
    t <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay t
    return (year, month, day)

main = do
    (year, month, day) <- getCurrentDate
    putStrLn $ "Today is " ++ show year ++ "/" ++ show month ++ "/" ++ show day
```

**出力:**

今日は 2021/11/25 です。

## [深く潜る]

(1) 歴史的な背景:

日付や時刻の処理は、プログラミングにおいて重要な機能です。Haskellには、日付や時刻を扱うための標準モジュールが用意されており、データ型や関数が豊富に用意されています。

(2) 代替方法:

Haskell以外にも、PythonやJavaScriptのような他の言語でも日付や時刻を取得することができます。また、「System.Time」モジュールを使用する方法もありますが、推奨されていません。

(3) 実装の詳細:

```getCurrentDate```関数は、```Data.Time.Clock```, ```Data.Time.Calendar```, ```Data.Time.LocalTime```, ```System.Locale```の4つのモジュールを使用しています。また、関数の定義には、```IO```モナドを使用していることに注意してください。

## [参考リンク]

- [Haskell公式ドキュメント](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/)
- [日付と時刻の取得方法についてのStackOverflowの質問](https://stackoverflow.com/questions/14930054/haskell-how-can-i-get-a-current-day-month-year)
- [Hoogleで検索する場合2021/11/25](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html#g:4)
- [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/Modules)