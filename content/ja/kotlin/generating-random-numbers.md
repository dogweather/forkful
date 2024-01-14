---
title:                "Kotlin: ランダムな数字の生成"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することによるメリットは、プログラミングの世界で様々なアプリケーションやゲームを作ることにあります。例えば、乱数を使ってゲームの結果を決めたり、ランダムに選ばれたデータを表示したりすることができます。

## 方法

ランダムな数字を生成するには、Kotlinで提供される`Random`クラスを使用します。以下のコードを参考にしてください。

```Kotlin

// `Random`クラスのインスタンスを作成
val random = Random()

// 0から9までのランダムな数字を生成
val randomNumber = random.nextInt(10)
println(randomNumber) // 出力例：6

// ランダムなブール値を生成
val randomBoolean = random.nextBoolean()
println(randomBoolean) // 出力例：true

// 2つの整数値の範囲内でランダムな数字を生成
val randomInRange = random.nextInt(5, 10)
println(randomInRange) // 出力例：8

```

`Random`クラスは、ランダムな数字だけでなく、ランダムなブール値や範囲内の数字を生成することもできます。詳細な使い方については、公式ドキュメントをご覧ください。

## ディープダイブ

ランダムな数字を生成する方法は様々ありますが、実際にはコンピューターは完全にランダムな数字を生成することはできません。そのため、ランダムな数字生成のアルゴリズムにはさまざまな考え方があります。

例えば、一般的なアルゴリズムの一つは、乱数生成器と呼ばれるものです。これは、自分で算出したシード値を元に生成された疑似乱数を使用して数字を生成します。その結果、同じシード値を使用すれば同じ数字を生成することができます。そのため、一見ランダムな数字に見えるものでも、実際には規則性があります。

また、その他にも様々な数学的アルゴリズムを使用する方法があります。詳細な情報については、ランダムな数字生成に関する研究データや書籍を参考にしてください。

## もっと詳しく知りたい方はこちら

- [Kotlin 公式ドキュメント](https://kotlinlang.org/docs/reference/basic-types.html#randomized-numbers)
- [ランダムな数字生成についての詳細な説明](https://www.geeksforgeeks.org/generating-random-numbers-in-kotlin/)
- [乱数生成器についての説明](https://en.wikipedia.org/wiki/Random_number_generation)