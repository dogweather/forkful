---
title:                "Clojure: 「部分文字列の抽出」"
simple_title:         "「部分文字列の抽出」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列からサブストリングを抽出することに取り組む理由はさまざまです。例えば、特定の文字列パターンを持つ単語を抽出する場合や、長い文字列から特定の部分を抽出する場合などがあります。Clojureでは、便利な関数が用意されているので、効率的にサブストリングを抽出することができます。

## 方法

Clojureでサブストリングを抽出するには、`subs`関数を使用します。`subs`関数は、引数として文字列、開始位置、終了位置を取ります。例えば、次のコードは、文字列から2番目から4番目までの文字を抽出します。

```Clojure
(def str "Hello World")
(subs str 1 4) ;=> "ell"
```

また、`subs`関数を使用する際には、文字列の長さを超える位置を指定することも可能です。その場合は、最後の文字までが抽出されます。

```Clojure
(subs str 5 10) ;=> " World"
(subs str 5 100) ;=> " World"
```

さらに、正規表現を使用して文字列パターンを指定することもできます。正規表現を使用する場合は、`re-find`関数を使用します。次のコードは、文字列から数字のみを抽出します。

```Clojure
(def str "I am 23 years old")
(re-find #"\d+" str) ;=> "23"
```

## ディープダイブ

サブストリングを扱う際には、文字列の位置を正確に把握することが重要です。Clojureは、0から始まるインデックスを使用します。つまり、文字列 "Hello" の最初の文字は、0番目の位置にあります。また、`subs`関数では、開始位置は含まれるが、終了位置は含まれないことにも注意が必要です。

さらに、正規表現を使用する際には、適切なパターンを指定することが重要です。正規表現を使用する場合は、`#"pattern"`のように、パターンをクォートする必要があります。また、`re-find`関数は、文字列内から最初の一致を見つけるため、注意して使用する必要があります。

## お役立ちリンク

- [Clojure 公式ドキュメント (日本語版)](https://clojure-doc.org/index_ja.html)
- [正規表現チートシート](https://www.atmarkit.co.jp/ait/subtop/features/di/cheat_sheet/01.html)
- [Clojureの正規表現について学ぶ](https://clojure.or.orj/pdf/ClojureRegularExpressions.pdf)