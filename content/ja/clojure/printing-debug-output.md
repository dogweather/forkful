---
title:                "「デバッグ出力のプリント」"
html_title:           "Clojure: 「デバッグ出力のプリント」"
simple_title:         "「デバッグ出力のプリント」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力をプリントすることの目的は、コードの実行中に特定の値や処理の結果を確認することができるからです。これにより、コードの動作をより理解し、バグを特定することができます。

## プリントする方法

```Clojure
(prn "Hello, world!")
```
```
"Hello, world!"
```

デバッグ出力を行うためには、 `prn` 関数を使います。この関数は、与えられた値を文字列として表示します。 `prn` は `println` と似ていますが、自動的に改行を追加しない点が異なります。このことが、関数や関数内の値を確認する際に便利です。また、 `prn` は任意の数の引数を受け取ることができます。

## ディープダイブ

 `prn` 関数は、実際には `println` 関数のラッパーであり、 `print` 関数と `newline` 関数を組み合わせて動作します。したがって、 `prn` を使う際には、 `print` や `newline` を直接使うこともできます。また、 `pr` という別の関数もあります。この関数は、 `prn` と同じく値を文字列として表示しますが、値をプリントする前に `pr-str` 関数を使って文字列に変換する点が異なります。

## 関連リンク

- [Clojureドキュメント](https://clojure.org/)
- [Clojureチュートリアル](https://clojure.org/guides/getting_started)
- [ClojureScript公式サイト](https://clojurescript.org/)
- [Clojureプログラミングの基礎](https://clojure-doc.org/articles/tutorials/introduction.html)