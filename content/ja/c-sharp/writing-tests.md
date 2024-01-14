---
title:                "C#: テストを書く"
simple_title:         "テストを書く"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ？
プログラミングにおいて、テストは非常に重要です。テストを書くことで、ソフトウェアのバグを検出しやすくなり、コードの品質を向上させることができます。

## 方法
テストを書くには、まずテストフレームワークを選択します。本記事では、C#で書かれたテストを実行するための「NUnit」を使用します。以下のコードを参考に、どのようにNUnitを使用するかを説明します。

```C#
// まず、必要なNUnitライブラリをインストールする必要があります。
using NUnit.Framework;

// 以下のように、テストクラスを作成します。
public class CalculatorTests
{
    // テストケースごとに、[Test]属性を付けたメソッドを作成します。
    // メソッドの名前は適宜変更してください。
    [Test]
    public void Add_TwoIntegers_ReturnsSum()
    {
        // テストするメソッドを呼び出し、結果を変数に代入します。
        int result = Calculator.Add(3, 5);

        // Assertクラスを使用して、テストケースの期待値と実際の結果を比較します。
        // テストが成功した場合は何も起こりませんが、失敗した場合は例外が発生します。
        Assert.AreEqual(8, result);
    }
}
```

上記のように、NUnitを使用してテストを書くことで、コードが期待通りに動作するかを簡単に確認することができます。

## 深く掘り下げる
テストを書く際には、いくつかのルールに従うことが重要です。例えば、テストケースごとにメソッドを作成し、テストするデータに応じて名前を付けること、断言する値について明確にすることなどが挙げられます。また、テストカバレッジと呼ばれる概念を使用して、コードのどの部分をテストしているかを可視化することも重要です。

## 参考になるリンク
- [NUnit公式サイト](https://nunit.org/)
- [NUnitドキュメント](https://docs.nunit.org/)
- [テストカバレッジについての記事（英語）](https://www.softwaretestinghelp.com/test-coverage-measurement-in-software-testing/)