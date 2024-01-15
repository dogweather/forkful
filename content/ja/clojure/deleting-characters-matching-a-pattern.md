---
title:                "パターンにマッチする文字の削除"
html_title:           "Clojure: パターンにマッチする文字の削除"
simple_title:         "パターンにマッチする文字の削除"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Clojure *最新バージョン*プログラミングの記事を書いて、日本語読者向けのカジュアルなトーンと非冗長なスタイルで追及する。"## なぜ", "## 方法", "## 深層"の3つのセクションに分け、それぞれ日本語で翻訳する。

## なぜ

パターンにマッチする文字を削除する理由は、データの整理や操作を目的としたプログラミングにおいて非常に有用です。例えば、ファイル読み込み時に不要な文字を削除することで、データの構造をより明確にすることができます。

## 方法

Clojureでは、文字列を操作するための便利な関数が多数用意されています。その中でも、特定のパターンにマッチする文字を削除する方法を紹介します。

まずは、`clojure.string`ライブラリをインポートします。

```Clojure
(require '[clojure.string :as str])
```

次に、`str`関数を使って文字列を作成します。

```Clojure
(def sample-str "Hello, World!!!")
```

この文字列から、英数字以外の文字を削除するには、`replace`関数を使用します。ここでは、正規表現を使ってマッチングさせます。

```Clojure
(str/replace sample-str #"[^\w\s]" "")
```

上記のコードを実行すると、以下のような結果が得られます。

```Clojure
"Hello World"
```

これで、英数字以外の文字が削除され、整形された文字列を取得することができました。

## 深層

Clojureでは、文字列を操作するために正規表現を使うことができます。`str`や`replace`のような関数を組み合わせることで、より複雑な操作も可能です。また、`clojure.string`ライブラリには他にも便利な関数が多数用意されているので、ぜひ活用してみてください。

## See Also

- [Clojure string functions](https://clojuredocs.org/clojure.string)
- [Regular expressions in Clojure](https://clojuredocs.org/clojure.core/re-matches)