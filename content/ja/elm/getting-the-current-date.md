---
title:    "Elm: 現在の日付を取得する"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得することの魅力をご存知ですか？Elmでプログラミングをする際に、現在の日付を取得する必要がある場合があります。その理由を紹介します。

## 方法

```Elm
import Html exposing (text)
import Time exposing (now)

main =
  now
    |> Time.toUtc
    |> Time.format "%Y/%m/%d"
    |> text
```

このように、`Time`をインポートし、`now`を使用して現在の日時を取得します。そして、`toUtc`を使ってUTC時間に変換し、`format`を使って表示形式を指定します。最後に、`text`を使ってHTMLに変換して表示します。

このコードを実行すると、現在の日付が表示されます。例えば、2021年10月23日ならば、"2021/10/23"という形式で表示されます。

## ディープダイブ

日付を取得する際には、タイムゾーンに注意する必要があります。例えば、上記のコードでは`toUtc`を使用してUTC時間に変換していますが、現在地のタイムゾーンに合わせて変換することも可能です。また、`format`を使って表示形式を自由に変更することができます。

## 参考リンク

- [Official Elm Documentation - Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Learn Elm Programming - Getting the Current Date](https://elmbasics.com/getting-the-current-date)
- [Elm Japan User Group](https://elmjapan.org/)