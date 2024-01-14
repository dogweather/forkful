---
title:                "Kotlin: 「デバッグ出力のプリント」"
simple_title:         "「デバッグ出力のプリント」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜデバッグ出力を印刷するのか？

プログラミングを行う上で、デバッグは非常に重要な要素です。デバッグ出力を印刷することで、プログラムの動作を詳しく理解することができ、問題を解決するのに役立ちます。

## デバッグ出力を印刷する方法

デバッグ出力を印刷する方法は非常に簡単です。まず、`println()`関数を使って出力したいデータを指定します。以下は、Kotlinでの例です。

```Kotlin
fun main(args: Array<String>) {
    // デバッグ用の変数
    val num1 = 10
    val num2 = 20

    // 変数の値を出力する
    println("num1の値は $num1 です。")
    println("num2の値は $num2 です。")

    // 合計を出力する
    val sum = num1 + num2
    println("num1 + num2 の合計は $sum です。")
}
```

上記のコードを実行すると、以下のようにデバッグ出力が表示されます。

```
num1の値は 10 です。
num2の値は 20 です。
num1 + num2 の合計は 30 です。
```

また、Kotlinでは`print()`関数を使うことで、改行なしの出力も可能です。

```Kotlin
fun main(args: Array<String>) {
    print("Hello")
    print("World")
}
```

上記のコードを実行すると`HelloWorld`という出力が得られます。

## デバッグ出力の深い掘り下げ

デバッグ出力はプログラムを理解する上で非常に役立ちます。例えば、上記のコードの`sum`の値が意図したものと一致していない場合、どこで間違っているのかを特定するのにデバッグ出力が役立ちます。また、デバッグ出力を使うことで、変数の値や条件分岐の結果など、プログラムの様々な部分の動作を確認することができます。

さらに、デバッグ出力の代わりにデバッガーを使うこともできます。デバッガーを使うと、ステップ実行や変数の値の監視など、より詳細なデバッグが可能になります。しかし、デバッガーを使うには専門的な知識が必要です。その点、デバッグ出力は誰でも簡単に活用することができます。

## 同じく見ておきたい

より詳しくデバッグ出力を活用する方法については、以下のリンク先を参考にしてみてください。

- [Kotlinの公式ドキュメント](https://kotlinlang.org/docs/tutorials/kotlin-for-py/creating-classes.html)
- [Androidアプリのデバッグ法 - デバッグを活用して品質を向上させる方法](https://developer.android.com/studio/debug)