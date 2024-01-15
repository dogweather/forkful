---
title:                "テストの作成"
html_title:           "PHP: テストの作成"
simple_title:         "テストの作成"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか
プログラミングにとって、テストは非常に重要です。テストを書くことで、コードの品質をチェックし、将来的な変更やバグ修正にも自信を持って取り組むことができます。

## テストを書く方法
テストを書くには、PHPUnitなどのテストフレームワークを使用するのが一般的です。以下の例は、簡単な掛け算関数のテストを書く方法を示しています。

```PHP
<?php

// 掛け算関数を定義する
function multiply($a, $b) {
  return $a * $b;
}

// テストを書くためのクラスを作成する
class MultiplyTest extends \PHPUnit\Framework\TestCase {
  // 掛け算関数のテストメソッドを定義する
  public function testMultiply() {
    // テストする数値を用意する
    $inputA = 5;
    $inputB = 10;
    $expectedOutput = 50;

    // 掛け算関数を実行し、結果を変数に代入する
    $output = multiply($inputA, $inputB);

    // 期待する結果と実際の結果が一致することを確認する
    $this->assertEquals($expectedOutput, $output);
  }
}

```

実行結果は以下のようになります。

```
PHPUnit 9.5.2 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:00.008, Memory: 4.00 MB

OK (1 test, 1 assertion)
```

このように、テストがOKであれば「OK」と表示されます。

## テストの深層へ
テストはテストコードを書くだけではなく、テストのカバレッジやテスト駆動開発（TDD）など、さまざまなアプローチがあります。また、自動化されたテストは継続的インテグレーション（CI）や継続的デリバリー（CD）にも必要不可欠です。

## おすすめリンク
- PHPUnit: https://phpunit.de/
- テスト駆動開発入門: https://www.ogis-ri.co.jp/otc/hiroba/technicalguidance/TestDrivenDevelopment.pdf
- 継続的インテグレーションと継続的デリバリーの概要: https://qiita.com/wataruoguchi/items/5ca37779b8d0dda55e9f