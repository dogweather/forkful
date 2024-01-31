---
title:                "テストの作成"
date:                  2024-01-19
simple_title:         "テストの作成"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストの書き方とその理由)
テストとは、プログラムが期待通りに動作することを保証するためのコードです。品質を保ち、将来の変更で不具合が起きないようにするため、プログラマはテストを書きます。

## How to: (やり方)
PHPでテストを書くには、PHPUnitというツールを使うのが一般的です。以下は簡単なテストの例です。

```PHP
<?php
use PHPUnit\Framework\TestCase;

class SampleTest extends TestCase
{
    public function testAddition()
    {
        $this->assertEquals(4, 2 + 2);
    }
}
```

実行結果:

```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:00.020, Memory: 6.00 MB

OK (1 test, 1 assertion)
```

## Deep Dive (詳細情報)
PHPUnitは2001年にSebastian Bergmannによって作成されました。代替手段には、BehatやPHPSpecなどが存在します。PHPUnitでは、アサーションを使用して期待値と実際の値を比較し、テストの結果を決定します。テスト駆動開発(TDD)やビヘイビア駆動開発(BDD)などのテスト方法論を利用する場合もあります。

## See Also (関連情報)
- [PHPUnit公式サイト](https://phpunit.de/)
- [テスト駆動開発(TDD)](https://en.wikipedia.org/wiki/Test-driven_development)
- [ビヘイビア駆動開発(BDD)](https://en.wikipedia.org/wiki/Behavior-driven_development)
