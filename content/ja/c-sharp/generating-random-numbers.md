---
title:                "乱数の生成"
date:                  2024-01-27T20:32:49.308080-07:00
model:                 gpt-4-0125-preview
simple_title:         "乱数の生成"

category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

C#でランダムな数を生成することは、指定された範囲内で予測不可能な数値を作り出すことを含みます。プログラマーは、予測不可能性が必要または実世界のランダム性をシミュレーションすることが求められる機能、例えば暗号化、シミュレーション、ゲームの実装にこれらの方法を使用します。

## どのようにして：

C#でランダムな数を生成する最も一般的な方法は、`System.Random` クラスを使用することです。こちらがその使用例を示すシンプルな例です：

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // 1と99の間の数を生成
        Console.WriteLine($"Random number: {randomNumber}");
    }
}
```

これは、例えば以下のようなランダムな数を出力します：

```
Random number: 42
```

0.0から1.0の間のランダムな浮動小数点数を生成するには、`NextDouble`メソッドを使用できます：

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Random double: {randomDouble}");
```

セキュリティに敏感なアプリケーションで暗号学的なランダム性が必要な場合は、`System.Security.Cryptography`にある`RNGCryptoServiceProvider`クラスを使用する方がよいでしょう：

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // 4バイトのランダム数を生成
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Cryptographically secure random number: {value}");
    }
}
```

## より詳しく

年々、C#におけるランダム数の生成は進化してきました。初めは、`System.Random`クラスが疑似ランダム数を生成するための主要な方法でした。特定のシード値が与えられると、それは同じ数のシーケンスを生み出しますので、これは疑似ランダムと言えます。これはデバッグやテストの再現性のために役立ちます。

基本的なニーズには十分ですが、`System.Random`はスレッドセーフではなく、予測可能な結果を生み出すことがあります。これはセキュリティに依存するアプリケーションには適していません。この制限により、より安全だがよりリソースを要する`RNGCryptoServiceProvider`が暗号学的なランダム性のために導入されました。

.NET Coreや.NET 5+では、ランダム数を安全に生成するためのより現代的で使いやすいオプションとして意図された`System.Security.Cryptography`の`RandomNumberGenerator`クラスが、`RNGCryptoServiceProvider`と比較して代替として登場しています。

C#でランダム数を生成する各方法は、アプリケーションの要件に応じてそれぞれの場所があります。ほとんどのアプリケーションでは`System.Random`で十分ですが、安全で予測不可能なランダム数が必要な場合、暗号学的クラスが強固な代替手段を提供します。
