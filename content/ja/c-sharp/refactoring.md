---
title:                "リファクタリング"
date:                  2024-01-26T01:17:28.520327-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"

category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/refactoring.md"
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
