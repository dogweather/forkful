---
title:    "Elm: 文字列の連結"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## なぜ

文字列を連結する必要性は、プログラミングにおいて非常に一般的です。例えば、ユーザーにとって分かりやすいメッセージを表示するために、複数の変数やテキストを結合する必要がある場合があります。Elmでは、文字列を連結するための便利な方法が提供されています。

## 方法

Elmでは、文字列を連結するために、2つの重要なメソッドがあります。それは `++` と `concat` です。まずは `++` を使用してみましょう。

```Elm
message = "天気は"
condition = "晴れです"
result = message ++ condition

-- 結果は"天気は晴れです"という文字列になります
```

次に、`concat` を使ってみましょう。

```Elm
parts = ["今日の", "天気は", "曇りです"]
result = String.concat parts

-- 結果は"今日の天気は曇りです"という文字列になります
```

どちらの方法も同じ結果が得られますが、使い方には微妙な違いがあります。 `++` は2つの文字列を結合するために使われ、 `concat` はリスト内のすべての要素を結合するために使われます。

## ディープダイブ

文字列を連結する際、重要なことは型の一致です。Elmでは、文字列を結合するためには `++` と `concat` で共に同じ型のデータを使う必要があります。例えば、 `++` では、文字列を結合する前に `append` 関数で文字列を Int 型に変換する必要があります。また、 `concat` ではリスト内のすべての要素が同じ型である必要があります。

## もっと詳しくは

`Text.join` 関数と `String.intercalate` 関数も文字列を連結するために使える便利な方法です。詳しくは、以下のリンクを参考にしてください。

- [Elm の文字列に関するドキュメント](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm in Japanese 公式ドキュメント](https://elm-jp.org/guide/elm/strings/)
- [Explore Elm: Concatenating Text](https://minima.dev/elm-in-japanese/series/explore-elm/concatenating-text/)