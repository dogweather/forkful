---
title:                "ランダムな数字を生成する"
html_title:           "Kotlin: ランダムな数字を生成する"
simple_title:         "ランダムな数字を生成する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することのメリットは、オンラインゲームやランダムな結果が求められるアプリケーションの開発、または暗号化などのセキュリティー関連の用途があるからです。

## 生成方法

まず、`Random()`をインスタンス化し、`nextInt()`メソッドを使用して指定した範囲内のランダムな数値を生成する方法があります。以下のコードは、1から10までのランダムな数値を出力する例です。

```Kotlin
val random = Random()
val randomNumber = random.nextInt(10) + 1
println(randomNumber) // Output: 5、10も含まれる可能性があります
```

また、`Random()`のコンストラクターにはシード値を指定することができます。これにより、同じシード値を使用すると同じ数値が生成されるため、再現性のあるランダムな数値を生成することができます。

```Kotlin
val random = Random(1234)
val randomNumber = random.nextInt(10)
println(randomNumber) // Output: 3
```

他にも、`random()`関数を使う方法もあります。これは指定した範囲内の浮動小数点数を返します。

```Kotlin
val randomNumber = (1..10).random()
println(randomNumber) // Output: 8.634293648
```

## ディープダイブ

ランダムな数値の生成には、擬似乱数ジェネレーター（Pseudo-Random Number Generator, PRNG）というものが利用されます。これは、完全にランダムな数値ではなく、ある種のアルゴリズムに基づいて計算された数値を生成するものです。

PRNGには、線形合同法やメルセンヌ・ツイスタ法などがありますが、Kotlinではデフォルトでメルセンヌ・ツイスタ法が使用されています。このアルゴリズムは、計算量と周期のバランスが良いため、一般的に使用されています。

## 関連リンク

- [Kotlin 公式ドキュメント - Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Kotlin 公式ドキュメント - Ranges](https://kotlinlang.org/docs/ranges.html)
- [コンピューターの疑似乱数 - Wikipedia](https://ja.wikipedia.org/wiki/%E3%83%A9%E3%83%B3%E3%83%80%E3%83%A0%E3%83%8F%E3%83%BC%E3%83%89%E3%82%B9%E3%83%86%E3%83%BC%E3%83%89%E6%B3%95)