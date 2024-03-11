---
date: 2024-01-26 01:17:28.520327-07:00
description: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306F\u3001\u65E2\u5B58\
  \u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30B3\u30FC\u30C9\u306E\u69CB\u9020\u3092\
  \u5916\u90E8\u306E\u632F\u308B\u821E\u3044\u3092\u5909\u3048\u305A\u306B\u518D\u69CB\
  \u7BC9\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u306E\u6574\u7406\u3001\u53EF\u8AAD\u6027\
  \u306E\u5411\u4E0A\u3001\u8907\u96D1\u3055\u306E\u524A\u6E1B\u3001\u304A\u3088\u3073\
  \u4FDD\u5B88\u6027\u306E\u5411\u4E0A\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.713278-06:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306F\u3001\u65E2\u5B58\
  \u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30B3\u30FC\u30C9\u306E\u69CB\u9020\u3092\
  \u5916\u90E8\u306E\u632F\u308B\u821E\u3044\u3092\u5909\u3048\u305A\u306B\u518D\u69CB\
  \u7BC9\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u306E\u6574\u7406\u3001\u53EF\u8AAD\u6027\
  \u306E\u5411\u4E0A\u3001\u8907\u96D1\u3055\u306E\u524A\u6E1B\u3001\u304A\u3088\u3073\
  \u4FDD\u5B88\u6027\u306E\u5411\u4E0A\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
---

{{< edit_this_page >}}

## 何となぜ？

リファクタリングは、既存のコンピュータコードの構造を外部の振る舞いを変えずに再構築するプロセスです。プログラマーは、コードの整理、可読性の向上、複雑さの削減、および保守性の向上のためにこれを行います。

## 方法:

配列の数値の合計を計算して表示する簡単なC#メソッドをリファクタリングしましょう:

リファクタリング前:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

リファクタリング後:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// 使用例:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

リファクタリングにより、関心の分離が行われ、`Calculator`クラスが任意の数値配列を受け入れられるようになり、LINQを使って合計計算をより簡潔にすることができました。

## 深掘り

リファクタリングはSmalltalkプログラミングコミュニティに起源を持ち、1990年代にマーティン・ファウラーの著書「Refactoring: Improving the Design of Existing Code」によって広まりました。年月を経るうちに、アジャイルメソッドロジーと良いコーディング習慣の基本的な部分へと成長してきました。

リファクタリングには様々なアプローチがあります。例えば、テスト駆動開発（TDD）のRed-Green-Refactorなどがあります。これは、失敗するテストから始めてテストをパスさせ、その後コードをきれいにすることで、リファクタリングがバグを導入しないことを保証します。

リファクタリングを実装する際には、プロセス中に機能が壊れていないことを保証するために包括的なテストスイートが重要です。ReSharperのようなC#用の自動リファクタリングツールもこのプロセスを支援することができます。これにより、コード構造を安全に変更する方法を提供します。しかし、ツールはコードベースとコーディング原則への深い理解を補完するものでなければなりません。

## 参考

- マーティン・ファウラーによるリファクタリングに関する基本的な著作: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Visual Studioにおけるリファクタリングに関するMicrosoftのガイド: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- 例付きのリファクタリングパターンの詳細な調査: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
