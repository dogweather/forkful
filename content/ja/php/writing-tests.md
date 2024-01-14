---
title:    "PHP: テストの作成"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜテストを書くのか

あなたはプログラミングをしているとき、何度も同じコードを書きたくありません。そのためには、バグを見つけて修正する必要があります。しかし、大規模なプロジェクトでは、バグの修正が非常に困難になる場合があります。こうした問題を回避するために、テストを書くことが重要です。

## 書き方

テストを書くには、PHPユニットテストフレームワークを使用します。これは、コードが期待どおりに動作するかどうかを確認するためのツールです。以下の例を参考にして、テストコードを書いてみましょう。

```PHP
class CalculatorTest extends PHPUnit_Framework_TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $result = $calculator->add(2, 3);
        $this->assertEquals(5, $result);
    }

    public function testSubtract()
    {
        $calculator = new Calculator();
        $result = $calculator->subtract(5, 2);
        $this->assertEquals(3, $result);
    }
}
```

上記の例では、Calculatorクラスのaddメソッドとsubtractメソッドをテストしています。テストコードでは、2つの数字を引数として渡し、期待する結果が得られるかどうかを確認しています。もしコードにバグがある場合は、エラーが発生します。

## ディープダイブ

テストを書く際には、様々な種類のテストがあります。ユニットテストの他にも、結合テストや受け入れテストなどがあります。それぞれのテストの目的や書き方を深く理解することで、より強固なコードを書くことができます。

## おわりに

テストを書くことはプログラミングにおいて非常に重要です。コードをテストすることで、バグを見つけて修正する手間を省くことができます。是非、今日からテストを書く習慣を身につけてみてください。

## 関連リンク

- [PHPユニットテストフレームワーク公式サイト](https://phpunit.de/)
- [ユニットテストとは？](https://qiita.com/tokikaze0604/items/f7a271d0a8c33f0fef97)
- [テスト駆動開発の基本](https://qiita.com/takafumi_nakahara/items/caf652b232de58a7e2ab)