---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ?

ランダム数字の生成は、予測できない数値をプログラム上で作り出すプロセスです。なぜプログラマはこれを行うのかと言えば、テストデータの生成やゲームの要素をランダム化するなど、多種多様な目的でそのニーズがあります。

## 実際にやってみよう:

```C#
using System;

class Program
{
    static void Main()
    {
        Random rnd = new Random();
        int number = rnd.Next();
        Console.WriteLine("生成されたランダム数値は " + number);

        number = rnd.Next(10);
        Console.WriteLine("0から9までのランダムな数値は " + number);

        number = rnd.Next(5, 15);
        Console.WriteLine("5から14までのランダムな数値は " + number);
    }
}
```

これはランダム数字を生成する一つの簡単な例です。上記のプログラムを実行すると下記のような結果が得られます。

```C#
生成されたランダム数値は 1152921504628320427
0から9までのランダムな数値は 4
5から14までのランダムな数値は 9
```

## 深掘り

生成されるランダムな数値の歴史的背景については、古くからゲームやシミュレーション、クリプトグラフィなどにおいて、不確実性を持つある地点を決定する手段として使われてきました。

乱数生成には、`Random` クラス以外にも、より安全な乱数(クリプトグラフィ的安全な乱数)を生成するための `RNGCryptoServiceProvider` クラスなどが存在します。

また、`Random` クラスの `Next` メソッドの場合、数値は0から`Int32.MaxValue`までの範囲で生成され、範囲を制限するために2つのパラメータを指定することも可能です。

## 参照情報

以下のリンクは、乱数生成についてさらに詳しく調査するための役に立つ情報です。

- Microsoft .NET Documentation: [Random Class](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- RNGCryptoServiceProvider Class: [RNGCryptoServiceProvider](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)
- Wikipedia on Random Number Generation: [Randomness](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0)