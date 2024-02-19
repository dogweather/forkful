---
aliases:
- /ja/c-sharp/generating-random-numbers/
date: 2024-01-27 20:32:49.308080-07:00
description: "C#\u3067\u30E9\u30F3\u30C0\u30E0\u306A\u6570\u3092\u751F\u6210\u3059\
  \u308B\u3053\u3068\u306F\u3001\u6307\u5B9A\u3055\u308C\u305F\u7BC4\u56F2\u5185\u3067\
  \u4E88\u6E2C\u4E0D\u53EF\u80FD\u306A\u6570\u5024\u3092\u4F5C\u308A\u51FA\u3059\u3053\
  \u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u4E88\u6E2C\u4E0D\u53EF\u80FD\u6027\u304C\u5FC5\u8981\u307E\u305F\u306F\u5B9F\
  \u4E16\u754C\u306E\u30E9\u30F3\u30C0\u30E0\u6027\u3092\u30B7\u30DF\u30E5\u30EC\u30FC\
  \u30B7\u30E7\u30F3\u3059\u308B\u3053\u3068\u304C\u6C42\u3081\u3089\u308C\u308B\u6A5F\
  \u80FD\u3001\u4F8B\u3048\u3070\u6697\u53F7\u5316\u3001\u30B7\u30DF\u30E5\u30EC\u30FC\
  \u30B7\u30E7\u30F3\u3001\u30B2\u30FC\u30E0\u306E\u5B9F\u88C5\u306B\u3053\u308C\u3089\
  \u306E\u65B9\u6CD5\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.913817
model: gpt-4-0125-preview
summary: "C#\u3067\u30E9\u30F3\u30C0\u30E0\u306A\u6570\u3092\u751F\u6210\u3059\u308B\
  \u3053\u3068\u306F\u3001\u6307\u5B9A\u3055\u308C\u305F\u7BC4\u56F2\u5185\u3067\u4E88\
  \u6E2C\u4E0D\u53EF\u80FD\u306A\u6570\u5024\u3092\u4F5C\u308A\u51FA\u3059\u3053\u3068\
  \u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u4E88\u6E2C\u4E0D\u53EF\u80FD\u6027\u304C\u5FC5\u8981\u307E\u305F\u306F\u5B9F\u4E16\
  \u754C\u306E\u30E9\u30F3\u30C0\u30E0\u6027\u3092\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\
  \u30E7\u30F3\u3059\u308B\u3053\u3068\u304C\u6C42\u3081\u3089\u308C\u308B\u6A5F\u80FD\
  \u3001\u4F8B\u3048\u3070\u6697\u53F7\u5316\u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\
  \u30E7\u30F3\u3001\u30B2\u30FC\u30E0\u306E\u5B9F\u88C5\u306B\u3053\u308C\u3089\u306E\
  \u65B9\u6CD5\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u4E71\u6570\u306E\u751F\u6210"
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
