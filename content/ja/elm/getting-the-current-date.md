---
title:                "「現在の日付を取得する」"
html_title:           "Elm: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 今日の日付は何ですか？
日付を取得するとは、現在の日付をプログラミングによって取得することです。プログラマーがこれを行う理由は、日付を使用してアプリケーションのロジックを制御したり、ユーザーに表示したりするためです。

## 方法：
Elmで現在の日付を取得する方法はいくつかあります。例えば、Dateモジュールを使う方法があります。以下の例を参考にして、コードブロック内にコーディング例とサンプル出力を記載します。

```
-- Dateモジュールを使った例
import Date exposing (today)

today --現在の日付を取得する
-- Output: Result Ok 2022 4 20
```

```
-- Dateモジュールを使わずに日付を取得する例
import Time exposing (Posix)
import Time.Date as Date
import Time.Extra exposing (millisToPosix)

Date.fromPosix (millisToPosix (Date.utcNow Posix))

-- Output: Result Ok 2022 4 20
```

## 詳細：
日付を取得する方法はプログラミング言語によって異なりますが、日付を管理するための標準的な機能を提供するモジュールやライブラリが多くあります。日付を取得するのに最適な方法を決めるために、プロジェクトの要件や使用する言語を考慮することが重要です。

代替手段としては、ブラウザのJavaScriptを使用して日付を取得する方法や、外部のAPIを使用して日付を取得する方法もあります。また、異なるタイムゾーンの日付を取得する場合は、Timeモジュールを使用してタイムゾーンを変換することができます。

## 関連情報：
- [Elm Dateモジュールのドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm Timeモジュールのドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Elm公式サイトの日付と時間の扱い方のガイド](https://guide.elm-lang.jp/dates_and_times/)