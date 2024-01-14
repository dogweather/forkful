---
title:                "PHP: テストを書く"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-tests.md"
---

{{< edit_this_page >}}

なぜテストを書くのか？

テストを書くことによって、コードの品質を向上させることができます。また、将来的にコードを変更した際にも、テストがあることで変更によってコードが壊れることを防ぐことができます。

## 書き方

テストを書くためには、まずはどのようなテストを書くかを決める必要があります。一般的に、ユニットテスト、統合テスト、機能テストの3つのレベルがあります。

ユニットテストは、個々のコンポーネントが期待通りに動作しているかをテストするものです。統合テストは、複数のコンポーネントが連携して正しく動作しているかをテストします。機能テストは、ユーザーが利用する機能が正しく動作するかをテストします。それぞれのレベルに合ったテストを書くことが重要です。

以下にPHPのコードを示します。

```PHP
// テスト用のクラスを呼び出す
require_once('calculator.php');

// テスト開始
class CalculatorTest extends PHPunit_Framework_TestCase {

    // テストケース１：加算のテスト
    public function testAdd() {
        // 期待結果
        $expected_result = 5;

        // テスト対象の関数を実行
        $actual_result = add(2, 3);

        // 結果をアサーション
        $this->assertEquals($expected_result, $actual_result);
     }

    // テストケース２：除算のテスト
    public function testDivide() {
        // 期待結果
        $expected_result = 2;

        // テスト対象の関数を実行
        $actual_result = divide(6, 3);

        // 結果をアサーション
        $this->assertEquals($expected_result, $actual_result);
     }
}
```

出力結果：

```
PHPUnit 6.1.4 by Sebastian Bergmann and contributors.

..                                                              2 / 2 (100%)

Time: 76 ms, Memory: 4.00MB

OK (2 tests, 2 assertions)
```

このように、テスト用のクラスを呼び出し、テストケースを作成してテスト対象の関数を実行し、期待結果と比較してテストが成功したかどうかをアサーションします。

## 詳細な情報

ユニットテストを書く際には、モックやスタブを使うことで外部依存を排除することができます。また、統合テストを行う際には、データベースや外部サービスを使用しない環境を用意することが重要です。

さらに、テストカバレッジを計測することで、どの程度のコードがテストされているかがわかります。テストカバレッジを向上させることで、より信頼性の高いコードを作ることができます。

## 併せて参照

- [PHPUnitドキュメント](https://phpunit.de/documentation.html)
- [テスト駆動開発](https://www.amazon.co.jp/%E3%83%86%E3%82%B9%E3%83%88%E9%A7%86%E5%8B%95%E9%96%8B%E7%99%BA-Kent-Beck/dp/4797321377)