---
title:                "Clojure: 正規表現の使用法"
simple_title:         "正規表現の使用法"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使う理由は非常に多くあります。例えば、文字列の検索や置換を効率的に行うことができます。また、データのパターンマッチングにも使われます。正規表現は柔軟なツールであり、多くのプログラミング言語でサポートされています。

## 使い方

正規表現はClojureで簡単に実装することができます。まず、`re-matches`関数を使って正規表現を表す文字列と検索対象の文字列を渡します。「Hello」のような文字列を入力すると、"Hello, World!"のような文字列でマッチングするかどうかを確認することができます。

```Clojure
(re-matches #"Hello" "Hello, World!")
;;=> "Hello"
(re-matches #"Hello" "こんにちは、世界！")
;;=> nil
```

更に、`re-seq`関数を使うことで、正規表現にマッチする部分文字列をすべて抽出することもできます。例えば、"Hello, 123!"という文字列から数字の部分だけを抽出するには、次のようにします。

```Clojure
(re-seq #"[0-9]+" "Hello, 123!")
;;=> ("123")
```

## ディープダイブ

正規表現は文字列のパターンマッチングにおいて非常に強力なツールです。しかし、正規表現をうまく使いこなすには、パターンの作成において少しの工夫が必要です。例えば、`.*`のようなワイルドカードを使うことで、任意の文字列を表すことができます。

## 参考リンク
- [正規表現チュートリアル](https://regexone.com)
- [Clojure正規表現ドキュメント](https://clojure.org/reference/regular_expressions)
- [正規表現の実践的な使い方](https://www.regular-expressions.info/tutorial.html)

# 参考文献