---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となく？何のために？
ランダムな数値生成は、予測不可能な数値を作り出すプロセスです。これにより、プログラム内での偶然性を再現したり、テストデータを作成したりすることができます。

## 作り方
Kotlinでは、`Random`クラスを使うことで簡単にランダムな数値を生成することができます。以下に例を示します。

```Kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt()
    println("Generated random number is $randomNumber")
}
```

上のコードを実行すると、以下のような出力を得ることができます。

```
Generated random number is 938248262
```

注意点としては、`Random.nextInt()`は任意の整数を返しますが、特定の範囲内でランダムな数値が欲しい場合は`Random.nextInt(from, until)`を使うことができます。

```Kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 10)
    println("Generated random number is $randomNumber")
}
```

## ディープダイブ
ランダム数生成は古くからコンピュータサイエンスに存在していました。しかし、完全にランダムな数列を生成するのは難しく、多くの技術では疑似ランダム数を使用します。これは一見ランダムに見えますが、実際には特定のアルゴリズムに基づいて生成されています。

Javaなど他のプログラミング言語では、特定の範囲内でランダムな数値を生成する方法がさまざまにありますが、Kotlinの`Random`クラスはそのユースケースをカバーしています。

なお、Kotlinのランダム数生成の内部では、ThreadLocalRandomやSplittableRandomなど、Javaのランダム生成APIを利用しています。

## 関連記事
- [Kotlin公式ドキュメンテーション - Randomクラス](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Javaでのランダム数生成](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [ランダムと疑似ランダムの違い](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)