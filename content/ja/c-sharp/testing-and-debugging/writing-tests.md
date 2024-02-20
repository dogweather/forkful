---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:21.228783-07:00
description: "\u2026"
lastmod: 2024-02-19 22:05:01.276534
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？

C#でテストを書くとは、コードの機能を検証するための自動スクリプトを作成することを意味し、それが期待通りに動作するかどうかを確かめるためです。プログラマーは、早期にバグを発見し、コードのリファクタリングを容易にし、新しい変更が既存の機能を壊さないようにするため、そしてそれによってソフトウェアの品質と信頼性を向上させるためにこれを行います。

## どのように：

C#開発者は、その柔軟性と広範な機能セットのために、NUnitまたはxUnitフレームワークを使用してテストを書くことが一般的です。こちらはNUnitを使用してシンプルな加算機能をテストする基本的な例です：

1. **NUnitとNUnit3TestAdapterをNuGetパッケージマネージャーや.NET CLI経由でインストールします**:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **まだ行っていない場合、C#クラスライブラリのプロジェクトを作成します**。

3. **テストするシンプルな関数を書きます**。例えば、`Calculator`というクラス内の加算メソッド:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **NUnitを使用してテストクラスを書きます**：
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **IDEのテストランナーや.NET CLIを使用してテストを実行します**：
```powershell
dotnet test
```

### サンプル出力：

テストが合格した場合、次のような出力が表示されるはずです：
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### xUnitを使用する場合：

xUnitを好む場合、セットアップはNUnitに似ています。こちらは`Calculator`クラスのテスト例をxUnitを使用して書き換える方法です：

1. **xUnitとxUnit.runner.visualstudioをインストールします**：
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **xUnitを使用してテストクラスを書きます**：
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **.NET CLIあるいはIDEの統合テストランナーを使用してテストを実行します**。

NUnitとxUnitの両方が、パラメータ化されたテスト、セットアップ/ティアダウン操作、テストのカテゴリ分けなどの強力な機能を提供するため、C#プログラマーのツールキットにとって、コードの品質と機能を確保するために欠かせないツールとなっています。
