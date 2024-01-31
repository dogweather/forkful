---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストの「何」と「なぜ」?)
テストコードはプログラムが正しい動作をする保証をするために書かれます。バグを早く見つけ、将来の機能追加やリファクタリングで発生するリスクを減らすためです。

## How to: (やり方)
C#のテストを書く一例を以下に示します。`NUnit` フレームワークを使った単体テストのサンプルです。

```C#
using NUnit.Framework;

namespace SampleTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_TwoNumbers_ReturnsSum()
        {
            // Arrange
            var calculator = new Calculator();
            int a = 5;
            int b = 7;

            // Act
            int result = calculator.Add(a, b);

            // Assert
            Assert.AreEqual(12, result);
        }
    }

    public class Calculator
    {
        public int Add(int a, int b)
        {
            return a + b;
        }
    }
}
```
実行すると、次のような出力が得られます。

```
Test Passed - Add_TwoNumbers_ReturnsSum
```

## Deep Dive (詳細解説)
テストの考え方はTDD（テスト駆動開発）から来ています。旧来は、コードを書いてからテストを作る流れでしたが、TDDではテストを先に書きます。代替手段としてはBDD（ビヘイビア駆動開発）があり、より行動にフォーカスを当てたテストが可能です。C#では、`NUnit`, `xUnit`, `MSTest` などのテストフレームワークが使われます。

## See Also (関連情報)
- [NUnit公式ドキュメント](https://nunit.org/docs/)
- [Microsoftの単体テストの概要](https://docs.microsoft.com/ja-jp/dotnet/core/testing/)
- [TDDに関するMartin Fowlerの記事](https://martinfowler.com/bliki/TestDrivenDevelopment.html)
