---
title:                "ランダム数の生成"
date:                  2024-01-20T17:48:55.950050-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

ランダム数生成は予測できない値を作ること。テスト、ゲーム、セキュリティなどに使う。必要なのは現実世界の不確実性をシミュレートすること。

## How to: (やり方)

```C#
using System;

public class RandomNumberGenerator
{
    static void Main()
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // 1 to 99
        Console.WriteLine($"Generated number: {randomNumber}");
    }
}

// Output
// Generated number: 42 (例として。毎回変わります)
```

## Deep Dive (掘り下げ)

C#の`System.Random`クラスは偽乱数ジェネレーター(Pseudo-Random Number Generator, PRNG)。実際には完全なランダムではないが、多くの用途に十分。歴史的には、早いコンピューター時代から発展。代替方法には、セキュリティが重要な場合の`System.Security.Cryptography.RNGCryptoServiceProvider`や、より均一な分布を求める場合の`System.Random`の他の実装がある。内部実装は、シード値からスタートし、数学的アルゴリズムを通じ連続する乱数を生成する。

## See Also (関連情報)

- 乱数生成に関する Microsoft のドキュメント: [Randomクラス (System)](https://docs.microsoft.com/ja-jp/dotnet/api/system.random)
- クリプトグラフィックに安全な乱数生成: [RNGCryptoServiceProviderクラス (System.Security.Cryptography)](https://docs.microsoft.com/ja-jp/dotnet/api/system.security.cryptography.rngcryptoserviceprovider)
- .NET におけるシード値と乱数の説明: [Random numbers in .NET (英語)](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0)
- 乱数生成の概念の解説: [ランダム性 (Wikipedia)](https://ja.wikipedia.org/wiki/%E3%83%A9%E3%83%B3%E3%83%80%E3%83%A0%E3%83%8D%E3%82%B9)
