---
title:                "デバッグ出力の印刷"
html_title:           "Kotlin: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何（ナニ） & どうして（ナゼ）？
デバッグ出力を表示することは、プログラマーがコードをテストするために行うことです。デバッグ出力を使用することで、コードの動作を理解し、問題を特定することができます。

## 方法：
以下に、Kotlinでデバッグ出力を表示する方法を示します。

1. メッセージを直接表示する方法：
```Kotlin
println("Hello World!")
```

2. 変数の値を出力する方法：
```Kotlin
val name = "John"
println("Name: $name")
```

3. 条件付きでメッセージを表示する方法：
```Kotlin
val num = 5
if (num < 10) {
	println("Number is less than 10!")
}
```

## 深く(DEEP DIVE)：
デバッグ出力は、1980年代にデバッガーの機能が限られていたために生まれました。デバッガーがデバッグ作業をサポートするようになったため、デバッグ出力はあまり使用されなくなっています。しかし、ビジュアルな方法ではなく、単純なテキスト形式でコードを理解することで、複雑な問題に取り組みやすい場合もあります。

代替方法としては、ロギングやデバッガーの使用があります。デバッグ出力は、単純な問題を解決するのに便利ですが、より複雑な問題ではロギングやデバッガーの方がより効果的です。

デバッグ出力を実現するために、Kotlinでは```println()```という組み込みの関数が使用されます。この関数は、コンソールにメッセージを出力することができます。

## 関連情報（SEE ALSO）：
- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/tutorials/command-line.html#using-the-command-line-to-run-kotlin-programs)
- [デバッグ入門ガイド](https://www.codecademy.com/articles/intro-to-debugging)