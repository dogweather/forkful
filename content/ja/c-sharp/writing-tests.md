---
title:                "C#: テストの書き方"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

##なぜテストを書くのか

プログラマにとって、テストは非常に重要です。テストを書くことで、コードの品質を保証し、バグを早期に発見することができます。さらに、テストを書くことでコードのリファクタリングや変更が容易になり、開発プロセスをスムーズに進めることができます。

##テストの書き方

テストを書くには、いくつかの方法がありますが、C#言語では「```C# ... ```」のコードブロックを使用することでテストを書くことができます。例えば、以下のようにテストを書くことができます。

```C#
// テスト用のクラス
public class Calculator {

    // 足し算のメソッド
    public int Add(int num1, int num2) {
        return num1 + num2;
    }
}

// テストクラス
[TestClass]
public class CalculatorTest {

    [TestMethod]
    public void TestAdd() {
        // インスタンスを作成
        Calculator calc = new Calculator();
        
        // テスト対象のメソッドを呼び出し
        int result = calc.Add(2, 3);
        
        // 期待値と実際の値を比較
        Assert.AreEqual(5, result);
    }
}
```

このように、テスト用のクラスとテストクラスを作成し、テスト対象のメソッドを呼び出し、期待値と実際の値を比較することでテストを実行することができます。

##テストの深堀り

テストを書く際には、いくつかのポイントに注意する必要があります。まず、テストケースを網羅的に書くことが重要です。あらゆるケースをテストすることで、バグを発見しやすくなります。また、テスト対象のメソッドの返り値やエラーハンドリングなど、細かい部分もテストすることでコードの安定性を保つことができます。さらに、テストコードもコードと同じようにリファクタリングすることで、テストの保守性を高めることができます。

##参考リンク

- [C#でのユニットテストの書き方](https://techacademy.jp/magazine/24963)
- [テスト駆動開発](https://ja.wikipedia.org/wiki/テスト駆動開発)
- [C#のテストフレームワーク「NUnit」公式サイト](https://nunit.org/)