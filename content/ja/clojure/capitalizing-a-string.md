---
title:                "Clojure: 「文字列を大文字化する」"
simple_title:         "「文字列を大文字化する」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Japanese "
## なぜ？

文字列の最初の文字を大文字に変換することは、日常的なプログラミングの中でしばしば行われるタスクです。例えば、名前などの情報を入力したとき、大文字で始まるように変換する必要があります。また、テキストを整形する際にも文字列の最初の文字を大文字にしたい場合があります。

## 方法

文字列の最初の文字を大文字に変換するには、Clojureのbuild-inの関数 "clojure.string/capitalize" を使用します。

```Clojure
(clojure.string/capitalize "clojure")
```
このコードの出力は、"Clojure" となります。

```Clojure
(clojure.string/capitalize "hello world")
```
このコードの出力は、"Hello world" となります。

## 詳細説明

この関数は、文字列の最初の文字を大文字に変換するだけでなく、2つ以上の単語を含む文字列の最初の文字も大文字に変換します。また、アクセント記号つきの文字や特殊文字にも対応しています。

```Clojure
(clojure.string/capitalize "こんにちは、clojure！")
```
このコードの出力は、"こんにちは、Clojure！" となります。

この関数は、文字列を変更するのではなく、新しい文字列を返すので、もとの変数に代入することなく使用することができます。

## 関連情報

[Lesson: 簡単な文字列操作 - Clojure](https://clojure.org/guides/learn/strings)

[標準ライブラリ参照 - clojure.string/capitalize](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/capitalize)

[関数と引数 - Clojureのドキュメント](https://clojure.org/guides/getting_started#_functions_and_arguments)