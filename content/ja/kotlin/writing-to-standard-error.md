---
title:                "Kotlin: 「標準エラーへの書き込み」"
simple_title:         "「標準エラーへの書き込み」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜコンソールのエラー表示をするのか

プログラミングをする際には、誤ったコードを書いたり、想定外のエラーが起きたりすることがあります。その際に、コンソールのエラー表示をすることで、どの部分が間違っているのか、どのようなエラーが起きているのかを確認することができます。そのため、エラー表示をすることはデバッグをする上でとても重要な役割を果たします。

## 書き方

Kotlinでは、`System.err`を使用して、コンソールにエラーを表示することができます。また、Kotlinの例外処理の中でも、例外をキャッチした際にエラーをコンソールに表示することができます。

```Kotlin
fun main() {
    try {
        // ここに起こりうるエラーを含むコードを書く
    } catch (e: Exception) {
        System.err.println("エラーが発生しました。: $e")
    }
}
```

上記の例では、`try-catch`ブロックを使用し、エラーが発生した場合に`System.err.println()`でエラーをコンソールに表示しています。

## 深堀り

Kotlinでは、標準ライブラリである`kotlin.io`を使用することで、さまざまな方法でエラーをコンソールに表示することができます。例えば、`println()`メソッドを使用することで、エラーを通常のテキストとして表示することができます。

```Kotlin
import kotlin.io.*

fun main() {
    println("エラーが発生しました。")
}
```

また、`e.printStackTrace()`を使用することで、エラーの詳細情報をコンソールに表示することもできます。

```Kotlin
fun main() {
    try {
        // ここに起こりうるエラーを含むコードを書く
    } catch (e: Exception) {
        e.printStackTrace()
    }
}
```

このように、Kotlinではさまざまな方法でエラーをコンソールに表示することができます。これらをうまく活用することで、より効率的なデバッグを行うことができます。

## 下記リンクも参考にしてみてください。

- [Kotlin公式ドキュメント：標準ライブラリの使用](https://kotlinlang.org/docs/reference/basic-types.html#using-kotlin-standard-library)
- [Kotlin公式ドキュメント：例外処理](https://kotlinlang.org/docs/reference/exceptions.html)
- [Kotlin入門編：例外処理の基本](https://www.tohoho-web.com/kotlin/basic/exception.html)