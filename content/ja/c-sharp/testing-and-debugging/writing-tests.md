---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:21.228783-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.128572-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3068\u306F\u3001\u30B3\u30FC\
  \u30C9\u306E\u6A5F\u80FD\u3092\u691C\u8A3C\u3059\u308B\u305F\u3081\u306E\u81EA\u52D5\
  \u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3092\u610F\
  \u5473\u3057\u3001\u305D\u308C\u304C\u671F\u5F85\u901A\u308A\u306B\u52D5\u4F5C\u3059\
  \u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u304B\u3081\u308B\u305F\u3081\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u65E9\u671F\u306B\u30D0\u30B0\
  \u3092\u767A\u898B\u3057\u3001\u30B3\u30FC\u30C9\u306E\u30EA\u30D5\u30A1\u30AF\u30BF\
  \u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3057\u3001\u65B0\u3057\u3044\u5909\u66F4\
  \u304C\u65E2\u5B58\u306E\u6A5F\u80FD\u3092\u58CA\u3055\u306A\u3044\u3088\u3046\u306B\
  \u3059\u308B\u305F\u3081\u3001\u305D\u3057\u3066\u305D\u308C\u306B\u3088\u3063\u3066\
  \u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u306E\u54C1\u8CEA\u3068\u4FE1\u983C\u6027\u3092\
  \u5411\u4E0A\u3055\u305B\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002."
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
