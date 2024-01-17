---
title:                "部分文字列を抽出する"
html_title:           "Clojure: 部分文字列を抽出する"
simple_title:         "部分文字列を抽出する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## なに？なぜ？

文字列から部分文字列を抽出することは、プログラマーがよく行うタスクです。これにより、特定のテキストパターンを含む文字列を検索し、必要に応じて変更することができます。

## やり方：

```Clojure
(str/split "こんにちは、私はClojureを勉強しています。" #"、")
```

このコードの出力は `("こんにちは" "私はClojureを勉強しています。")` となります。文字列を `split` 関数に渡すことで、指定したパターン（ここでは `,`）で文字列が分割されます。

```Clojure
(str/replace "Clojure is fun!" #"fun" "awesome")
```

このコードの出力は `"Clojure is awesome!"` となります。`replace` 関数により、指定したパターン（ここでは `fun`）が文字列内で見つかった場合に、別の文字列（ここでは `awesome`）に置き換えられます。

## 深堀り：

部分文字列を抽出する機能は、文字列処理に欠かせないものです。例えば、ユーザーからの入力情報を確認するフォームなどでは、必要な情報を部分文字列として抽出する必要があります。Clojureでは、上記のコードの他にも`subs`関数や`re-find`関数など、複数の方法で部分文字列を抽出することができます。

また、正規表現を使うことで、より複雑なパターンの文字列を抽出したり置き換えたりすることができます。ただし、正規表現を扱うには少し学習が必要です。

## 関連情報：

部分文字列の抽出や置換について、詳しい情報はClojure公式ドキュメントを参照してください。また、同様の機能を持つ他の言語やライブラリもありますので、状況に応じてどの方法が最適か検討してみてください。