---
title:                "パターンに一致する文字を削除する"
html_title:           "Kotlin: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何? & なぜ?
文字列内のパターンにマッチする文字を削除することは、プログラマーが文字列処理を行う際によく使われるテクニックです。文字列内の特定の文字を削除することで、より簡潔で効率的なコードを作ることができます。

## 方法:
以下に、「abc」という文字列から「b」を削除する方法を示します。 

```Kotlin
val str = "abc"
val result = str.replace("b", "")
println(result) // "ac"
```

## 詳細を深く調べる:
文字列のパターンマッチングは、主に正規表現と呼ばれる記法を使用して行われます。正規表現を使用することで、より柔軟なパターンにマッチする文字を削除することができます。

また、文字列のパターンマッチングは、プログラミング言語によって異なる実装方法があります。例えば、JavaではStringクラスのメソッドを使用することでパターンマッチングを行いますが、Kotlinでは正規表現を直接サポートしているのでより簡単に行うことができます。

## 関連情報:
- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/reference/strings.html#string-regular-expressions)
- [正規表現についての入門記事](https://www.ibm.com/developerworks/jp/linux/library/l-regexp.html)