---
title:                "現在の日付の取得"
html_title:           "Elm: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得することの魅力は、日常的なタスクであるため、Elmプログラミング初心者にとって便利なスキルとなります。

## 方法

```Elm
import Time exposing (now)

date : Task x Date
date =
  now
    |> Task.attempt identity
    |> Task.map .fromJust
```

上記のコードは、現在の日付を取得するために必要な最小限のコードです。```Time```モジュールから```now```をインポートし、```Task.attempt```を使用して日付を取得し、```Task.map```で結果を取り出します。```date```という名前のタスクを定義し、コンパイラーに任される動作を示します。

## ディープダイブ

```Time```モジュールにはさまざまな関数があり、現在の時刻や日付のほかにもタイムゾーンや時差を扱うことができます。また、自動的に変化する時刻を取得するためにも使用することができます。詳細な情報は、公式ドキュメントを参照してください。

## おすすめリンク

- [Elm公式ドキュメント](https://elm-lang.org/docs)
- [Elmメッセージャーの使い方](https://qiita.com/kiiik/items/66a411dc71d0d45e963b)