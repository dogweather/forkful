---
title:    "Java: ランダムな数字の生成"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数値を生成することの意義は何でしょうか？プログラミングでは、さまざまな場面でランダムな数値を必要とすることがあります。例えば、ゲーム開発ではダイスやカードのようなランダムな要素を含むゲームプレイを実現するために、またシミュレーションやテストなどでランダムなデータを使用することがあります。そこでJavaでは、簡単にランダムな数値を生成する方法が提供されています。次のセクションでは、その方法を詳しく見ていきましょう。

## ランダムな数値の生成方法

Javaでランダムな数値を生成するには、Randomクラスを使用します。以下の例では、nextInt()メソッドを使って0から10の間のランダムな整数を生成し、それをコンソールに出力しています。

```Java
Random random = new Random();
int randomNumber = random.nextInt(11);
System.out.println("ランダムな数値: " + randomNumber);
```

また、nextDouble()メソッドを使うことで、0から1の間のランダムな小数を生成することもできます。

```Java
double randomDouble = random.nextDouble();
System.out.println("ランダムな小数: " + randomDouble);
```

さらに、Randomクラスのコンストラクタにシード値を渡すことで、同じランダムな数値のシーケンスを生成することもできます。

```Java
long seed = 12345; // シード値
Random random = new Random(seed);
// 同じシード値を使っているので、同じシーケンスの数値が生成される
int randomNumber1 = random.nextInt(100);
int randomNumber2 = random.nextInt(100);
System.out.println("ランダムな数値1: " + randomNumber1);
System.out.println("ランダムな数値2: " + randomNumber2);
```

## ランダムな数値の深堀り

Randomクラスで提供されているメソッドは、線形合同法というアルゴリズムを使用してランダムな数値を生成しています。このアルゴリズムでは、現在の時刻などの外部要因をシード値として使用し、そのシード値を基にして数値を生成します。そのため、特定のシード値を使うことで同じ数値の系列が生成されることがあります。また、ランダムな数値の生成には現在の操作システムのリソースも利用されるため、パフォーマンスに影響を与える可能性があります。そのため、繰り返し実行される処理であれば、同じRandomインスタンスを使い回すことでパフォーマンスを向上させることができます。

## さらに見る

* [Randomクラスドキュメント](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
* [ランダムな数値の生成方法 | Java入門](https://java-beginner.com/java-random-numbers/)
* [ランダムな数値の生成について知ろう | Qiita](https://qiita.com/youwht/items/a7941b4473e6a66b6d2c)