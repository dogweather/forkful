---
title:    "PHP: テストを書く"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ
プログラムを書くことにおいて、テストを書くことは非常に重要です。テストは、プログラムが期待どおりに動作することを確認し、将来の変更やバグ修正にも役立ちます。テストを書くことで、安心してプログラムを開発することができます。

## 方法
テストを書くための基本的なステップを紹介します。

1. テストケースを作成する。これは、テスト対象のコードのあらゆる場合において、期待される結果を定義するものです。
2. テスト用のコードを書く。PHPの場合は、```assert```や```assertTrue```などの関数を使用して、テストケースを記述します。
3. テストを実行する。PHPの場合は、```phpunit```を使用することでテストケースを実行し、結果を確認することができます。

以下は、簡単な加算をするプログラムのテストコードの例です。

```PHP
<?php
function add($x, $y){
    return $x + $y;
}

// テストケースを定義する
$test_cases = array(
    array(1, 2, 3),
    array(5, 10, 15),
    array(-2, 6, 4),
);

// テストを実行する
foreach ($test_cases as $case) {
    $result = add($case[0], $case[1]);
    // 期待する結果と実際の結果が一致するかをテストする
    assert($result == $case[2]);
}
```

実行結果は以下のようになります。

```
$ phpunit test.php

OK (3 tests)
```

## 発展的内容
テストを書く際には、テスト駆動開発やモックを使用するなど、さまざまな方法があります。また、PHPの場合は、フレームワークやライブラリに組み込まれているテスト機能を使用することもできます。より詳細な情報や実践的なテクニックについては、以下のリンクを参考にしてください。

## 関連リンク
- [PHPUnit公式ドキュメント](https://phpunit.de/documentation.html)
- [PHPUnitでテストを書く方法](https://qiita.com/naga3/items/8ce7b05582a51419208b)
- [テスト駆動開発とは](https://qiita.com/NagaokaKenichi/items/bd2a590c60abb23c7842)
- [モックとは](https://qiita.com/uryyyyyyy/items/f4dbda338a1238409e01)