---
title:                "文字列の最初の文字を大文字にする"
html_title:           "Clojure: 文字列の最初の文字を大文字にする"
simple_title:         "文字列の最初の文字を大文字にする"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字にする必要性について、最大2行の文章で説明する。

人々が文字列を大文字にする理由の一つは、見栄えや統一性のためです。例えば、プログラム内の変数や関数の命名規則を統一するために大文字化することがあります。

## やり方

「```Clojure
(str/capitalize "clojure programming")
```
のように、 `str/capitalize` 関数を使用することで文字列を大文字にすることができます。実行すると、 `Clojure programming` という結果が得られるはずです。

また、特定の言語における文字列の大文字化ルールを反映させるために、 `str/upper-case` 関数を使用することもできます。例えば、日本語の場合は `str/upper-case :locale java.util.Locale/JAPANESE` のように指定することができます。

## ディープダイブ

`str/capitalize` 関数は、与えられた文字列の最初の文字を大文字に変換することで文字列を大文字にします。そのため、文字列の最初が数字や記号などの場合、そのまま変換されません。

また、文字列の中にアクセント記号や異なる言語の文字が含まれている場合、 `str/capitalize` 関数では正確な結果を得ることができないかもしれません。このような場合は、外部ライブラリを使用するなどの対応が必要になるかもしれません。

## 他の記事を参考にする

### [Clojure 公式ドキュメント - 文字列処理](https://clojuredocs.org/clojure.string)

### [Clojure 関数リファレンス - str/capitalize](https://clojuredocs.org/clojure.string/capitalize)

## 他の言語での実装もチェック

### [Python - capitalize()](https://docs.python.org/3/library/stdtypes.html#str.capitalize)

### [Java - String capitalize()](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#capitalize())