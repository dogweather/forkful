---
title:    "C#: 乱数生成"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
乱数を生成することについて考えると、それは楽しいプログラミングのチャレンジであり、ランダムな結果を必要とする多くのアプリケーションで役立つことができます。また、ランダム性はセキュリティや暗号化においても重要な要素であり、ランダム数を生成することは重要です。

## 生成する方法

C#言語でランダム数を生成するには、次のようなコードを使用することができます。

```C#
Random rand = new Random(); 
int randomNumber = rand.Next(); 
Console.WriteLine($"Random number: {randomNumber}");
```

上記のコードでは、`Random`クラスを使用して新しいランダム数を生成し、`Next()`メソッドを呼び出してランダムな整数を取得しています。また、`Next()`メソッドには最小値と最大値を指定することもでき、その範囲内でランダムな数を生成することができます。

## 深堀り

ランダム数を生成する際には、その数の分布にも注意を払う必要があります。C#言語では、`Random`クラスの代わりに`RNGCryptoServiceProvider`クラスを使用することで、より強力な乱数を生成することができます。また、乱数が必要なシチュエーションに応じて、異なるアルゴリズムやシード値を使用することもできます。

## 参考リンク
- [C# Random Number Generator](https://www.c-sharpcorner.com/UploadFile/mahesh/random-number-generator-in-C-Sharp/)
- [Generating Random Numbers in C#](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1)
- [SecureRandomクラスの使用](https://docs.microsoft.com/ja-jp/dotnet/api/system.security.cryptography.securerandom?view=netcore-3.1)

## その他の参考リンク
- [C#での乱数生成方法の比較](https://qiita.com/ohkawa/items/87b2a3cf9cae5d479ebf)
- [C#の乱数ジェネレーターは本当にランダムか？](https://www.atmarkit.co.jp/ait/articles/2008/10/news006.html)
- [乱数に関する注意点とは？](https://programmingacademy.jp/study/shuffle_guide/mistakes)