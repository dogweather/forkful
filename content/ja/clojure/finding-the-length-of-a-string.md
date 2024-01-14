---
title:    "Clojure: 文字列の長さを見つける"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# なぜ

文字列の長さを見つけることに取り組むのかを説明するために、しばらくお付き合いください。

## 方法

文字列の長さを見つけるには、 `count` 関数を使用する必要があります。以下のように、文字列を `count` 関数に渡して、文字の数を数えることができます。

```Clojure
(count "こんにちは") ; 出力結果: 5
```

これにより、カウント結果として `5` が返されます。同じ方法を使用して、変数に文字列を割り当ててから、その変数を `count` 関数に渡すこともできます。

```Clojure
(def greet "こんにちは")
(count greet) ; 出力結果: 5
```

## ディープダイブ

`count` 関数は、リストやベクターなどのコレクションに対しても使用することができます。リストを例にとると、リストに含まれる要素の数を数えるのに便利な方法です。例えば、次のようなリストがあるとします。

```Clojure
(def animals ["犬" "猫" "ウサギ" "ハムスター"])
```

このリストに対して、 `count` 関数を使用すると、以下のようになります。

```Clojure
(count animals) ; 出力結果: 4
```

また、 `count` 関数は `nil` の場合にも適用することができます。 `nil` は、値が存在しないことを示す特別なシンボルです。

```Clojure
(count nil) ; 出力結果: 0
```

## 参考

- [Clojureユーザーズガイド - 文字列](https://clojure.or.jp/document/guides/learn/read-string)
- [Clojure標準ライブラリ - count](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count)