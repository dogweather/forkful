---
title:    "Clojure: 部分文字列の取り出し"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

Substring（部分文字列）を抽出することの利点は、テキストから特定の部分を取り出すことができることです。この機能は、テキスト処理やデータ解析の際に非常に役立ちます。

## 方法

Clojureでは、文字列から部分文字列を抽出するための便利な関数があります。その1つが`subs`関数です。以下のように使用することができます。

```Clojure
(def text "こんにちは、世界")

(subs text 5 7)
```

上記のコードでは、変数`text`からインデックス5から7までの部分文字列（「世界」）を抽出しています。出力は`"世界"`になります。

もし指定したインデックスが文字列の長さを超えていた場合、自動的に最後の文字までを取得します。

```Clojure
(subs "Hello World!" 6 20)
```

上記のコードの出力は`"World!"`になります。

`subs`関数の他にも、`substring`や`split-at`などの関数も部分文字列を取得するのに役立ちます。

## 深堀り

部分文字列を抽出するにあたって、文字列のインデックスの仕組みを理解することが重要です。Clojureでは、文字列の最初の文字がインデックス0になります。また、インデックスは左から右に向かって0から始まり、右から左に向かって-1から始まります。

また、部分文字列を抽出する際には、String Index Out of Bounds（インデックスが範囲外）エラーに気をつける必要があります。これは、指定したインデックスが文字列の範囲を超えた場合に発生します。

## もっと見る

- [Clojureドキュメント](https://clojuredocs.org/clojure.core/subs)
- [Mastering Clojure Strings](https://purelyfunctional.tv/article/clojure-strings/)