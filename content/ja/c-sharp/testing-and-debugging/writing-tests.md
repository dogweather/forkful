---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:21.228783-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A C#\u958B\u767A\u8005\u306F\u3001\
  \u305D\u306E\u67D4\u8EDF\u6027\u3068\u5E83\u7BC4\u306A\u6A5F\u80FD\u30BB\u30C3\u30C8\
  \u306E\u305F\u3081\u306B\u3001NUnit\u307E\u305F\u306FxUnit\u30D5\u30EC\u30FC\u30E0\
  \u30EF\u30FC\u30AF\u3092\u4F7F\u7528\u3057\u3066\u30C6\u30B9\u30C8\u3092\u66F8\u304F\
  \u3053\u3068\u304C\u4E00\u822C\u7684\u3067\u3059\u3002\u3053\u3061\u3089\u306FNUnit\u3092\
  \u4F7F\u7528\u3057\u3066\u30B7\u30F3\u30D7\u30EB\u306A\u52A0\u7B97\u6A5F\u80FD\u3092\
  \u30C6\u30B9\u30C8\u3059\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3067\u3059\uFF1A 1.\u2026"
lastmod: '2024-04-05T21:53:43.002560-06:00'
model: gpt-4-0125-preview
summary: "**NUnit\u3068NUnit3TestAdapter\u3092NuGet\u30D1\u30C3\u30B1\u30FC\u30B8\u30DE\
  \u30CD\u30FC\u30B8\u30E3\u30FC\u3084.NET CLI\u7D4C\u7531\u3067\u30A4\u30F3\u30B9\
  \u30C8\u30FC\u30EB\u3057\u307E\u3059**."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
