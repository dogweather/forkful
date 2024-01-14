---
title:    "Kotlin: パターンに一致する文字を削除する"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

あなたはプログラミングをするとき、時々文字のパターンに合致する文字を削除したくなることがあります。この記事では、Kotlinを使って文字のパターンに合致する文字を削除する方法を紹介します。

## 方法

まず、削除したい文字列を含む変数を作成します。

```Kotlin
var sentence = "私はカフェでコーヒーを飲むのが好きです。"
```

次に、削除したい文字のパターンを正規表現で定義します。例えば、文字列から「カフェで」を削除したい場合、正規表現としては「カフェで」の部分を指定する必要があります。

```Kotlin
var pattern = "カフェで"
```

最後に、`replace()`関数を使って削除したい文字を指定し、新しい文字列を得ることができます。

```Kotlin
var newSentence = sentence.replace(pattern, "")
println(newSentence)
```

実行すると、以下のような結果が得られます。

```
私はコーヒーを飲むのが好きです。
```

## 詳細

`replace()`関数は、正規表現にマッチする文字列を削除するだけでなく、別の文字列に置き換えることもできます。また、`replace()`関数は文字列だけでなく、StringBuilderやRegexクラスオブジェクトでも使用することができます。

`replace()`関数の他にも、`replaceFirst()`や`replaceAfter()`などの関数があります。これらの関数は指定した文字列を最初の1つや最後の1つだけ置き換えることができます。詳しくは公式ドキュメントを参照してください。

## 関連記事

- [Kotlin String manipulation](https://dev.to/ecorrea/kotlin-string-manipulation-c6o)
- [Kotlin Regular Expressions](https://www.baeldung.com/kotlin-regular-expressions)
- [Kotlin Strings](https://kotlinlang.org/docs/basic-syntax.html#strings)