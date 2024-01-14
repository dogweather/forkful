---
title:                "Java: ランダムナンバーの生成"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数値を生成することの利点をご存知ですか？この記事では、なぜランダムな数値を生成することが重要であるかを説明します。

## ランダムな数値を生成する方法

Javaでは、ランダムな数値を生成するための便利な方法がたくさんあります。例えば、Mathクラスのrandom()メソッドやRandomクラスを使用する方法などがあります。下記にそれぞれのコーディング例とサンプル出力を掲載します。

```Java
// Mathクラスのrandom()メソッドを使用する例
double randomNumber = Math.random();
System.out.println("ランダムな数値：" + randomNumber);

// Randomクラスを使用する例
Random rand = new Random();
int randomInt = rand.nextInt(10);
System.out.println("10以下のランダムな整数：" + randomInt);
```

実行結果：

```
ランダムな数値：0.3736403225463508
10以下のランダムな整数：7
```

## ランダムな数値を生成する際の詳細

ランダムな数値を生成する際には、乱数ジェネレーターと呼ばれるアルゴリズムが使用されます。これらのアルゴリズムには、線形合同法やメルセンヌ・ツイスター法などがあります。ただし、これらのアルゴリズムは完全にランダムな数値を生成するわけではないので、注意が必要です。

## 参考リンク

- [Java Mathクラスのrandom()メソッドのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/Math.html#random--)
- [Java Randomクラスのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/util/Random.html)
- [乱数ジェネレーターについての詳細な解説](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0%E3%82%B8%E3%82%A7%E3%83%8D%E3%83%AC%E3%83%BC%E3%82%BF%E3%83%BC)

## 関連リンク

- [Javaの基本的なコーディングルール](https://qiita.com/opengl-8080/items/b78d101b1b371eee4d6a)
- [ランダムな数値を生成する他の方法](https://techacademy.jp/magazine/48091)