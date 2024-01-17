---
title:                "「ランダム数字の生成」"
html_title:           "C#: 「ランダム数字の生成」"
simple_title:         "「ランダム数字の生成」"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何をするのか & なぜ?: 
ランダムな数値を生成するとは何か、そしてなぜプログラマーがそれを行うのかを説明します。

## 方法: 
下記は、C#でランダムな数値を生成する方法の例と、その出力結果を```C# ...```のコードブロックで示します。

```C#
// ランダムな整数を生成する方法
Random rnd = new Random();
int randomNumber = rnd.Next();

// 結果: 574575
```

```C#
// 指定した範囲内のランダムな整数を生成する方法
Random rnd = new Random();
int randomNumber = rnd.Next(1, 100);

// 範囲: 1以上100未満 
// 結果: 52
```

```C#
// ランダムな小数を生成する方法
Random rnd = new Random();
double randomDecimal = rnd.NextDouble();

// 結果: 0.326147509218492
```

## 詳細: 
ランダムな数値を生成することは、プログラムの機能が実行される順番を制御するために使用されます。これにより、プログラムが予測できないように、毎回異なる結果を生成します。この機能は、ゲームや暗号化など幅広い用途で使用されています。生成する数値の種類や範囲を指定することもできます。

生成する数値には、疑似乱数と真の乱数の2種類があります。疑似乱数は、事前に決められた計算式によって生成されるため、厳密な意味でのランダムではありません。一方で、真の乱数は、外部の物理的要因（例：熱雑音や放射性崩壊）によって生成されるため、よりランダムな結果を得ることができます。

C#では、```Random```クラスを使用してランダムな数値を生成することができます。```Next()```メソッドを使用すると、指定の範囲内のランダムな整数を生成することができます。```NextDouble()```メソッドを使用すると、倍精度のランダムな小数を生成することができます。

## 関連情報: 
- [C# Randomクラスのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.random?view=net-5.0)
- [プログラムのランダム性](https://ja.wikipedia.org/wiki/%E3%83%A9%E3%83%B3%E3%83%80%E3%83%A0%E6%80%A7)