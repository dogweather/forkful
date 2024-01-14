---
title:    "Kotlin: 標準エラーへの書き込み"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ
プログラミングをする上で、標準エラー出力への書き込みが重要な役割を果たします。そのため、Kotlinでの標準エラー出力の書き込み方法について学ぶことは、プログラミングの基礎を固める上で必要不可欠です。

## 書き方
標準エラー出力への書き込みには、以下のようなコードを使用します。

```Kotlin
fun main() {
    val error = System.err
    error.println("このメッセージは標準エラー出力に表示されます。")
}
```

上記の例では、`System.err`を使用して標準エラー出力にアクセスし、`println()`メソッドを使ってメッセージを出力しています。また、`error`という変数を使用しているのは、プログラムの他の部分からも標準エラー出力を使用できるようにするためです。

## 深堀り
標準エラー出力には、プログラムの実行中に発生したエラーや警告メッセージを表示する場合に使用されます。通常は、発生したエラーが何故起きたのかを明確にするために、それに対応するプログラムコードを記述することで実現されます。

標準エラー出力への書き込みを使用すると、コンソールやログファイルなど、プログラムの実行環境に応じて異なる場所にメッセージを出力することができます。そして、これによりプログラムのデバッグや問題の特定を行うことが可能になります。

## 参考リンク
- [JavaCodeGeeks: Writing to stderr in Java](https://www.javacodegeeks.com/2015/09/writing-to-standard-error-in-java.html)
- [Kotlin Docs: System Streams](https://kotlinlang.org/docs/tutorials/command-line.html#system-streams)
- [freeCodeCamp: What's the Difference Between System.out and System.err](https://www.freecodecamp.org/news/whats-the-difference-between-system-out-println-and-system-err-println/)