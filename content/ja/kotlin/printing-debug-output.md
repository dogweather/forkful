---
title:                "Kotlin: 「デバッグ出力の印刷」"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ
プログラミングをする際に、デバッグ出力を印刷することの重要性がわからない人がいるかもしれません。しかし、実際にはデバッグ出力を印刷することで、プログラムの実行を追跡し、バグを見つけるための手がかりを得ることができます。

## 方法
```Kotlin
fun main() {
    val num1 = 5
    val num2 = 10
    println("num1の値は $num1 です")
    println("num2の値は $num2 です")
    println("num1とnum2の合計は ${num1 + num2} です")
}
```

実行すると、以下のような出力が得られます。
```
num1の値は 5 です
num2の値は 10 です
num1とnum2の合計は 15 です
```

このように、プログラムの特定の箇所で変数の値や計算結果を出力することで、プログラムの実行の流れを追うことができます。

## ディープダイブ
デバッグ出力には、変数の値や計算結果だけではなく、実行中の条件分岐やループの状態も出力することができます。これらの情報を出力することで、プログラムの制御フローを把握し、バグの原因を特定することができます。

また、デバッグ出力は単純なコードであればあるほど効果的になります。複雑なコードでは、出力の行数が増えて結果が見づらくなるため、適切な箇所にデバッグ出力を入れることが重要です。

## 併せて参照
- [Kotlin 公式ドキュメント](https://kotlinlang.org/)
- [プログラミング初心者へのアドバイス](https://www.youtube.com/watch?v=5V8f1L9-zeA)
- [デバッグ技術の基本](https://www.atmarkit.co.jp/fcoding/special/java_0808/java_0808_6.html)

デバッグ出力はプログラミングにおいて必要不可欠なスキルです。ぜひ積極的に活用して、プログラミングの力を高めていきましょう！