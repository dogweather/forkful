---
title:                "テストの作成"
aliases:
- /ja/php/writing-tests.md
date:                  2024-02-03T19:31:26.447635-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
プログラミングにおけるテストの記述とは、コードがさまざまな条件下で期待通りに動作することを確認するスクリプトを作成して実行することを指します。プログラマーは、品質を保証し、リグレッションを防止し、安全なリファクタリングを容易にするためにこれを行います。これは、健全でスケーラブルでバグのないコードベースを維持するために重要です。

## 方法：
### ネイティブPHP – PHPUnit
PHPでのテストに広く使用されているツールはPHPUnitです。Composer経由でインストールします：
```bash
composer require --dev phpunit/phpunit ^9
```

#### シンプルなテストの記述：
`tests`ディレクトリに`CalculatorTest.php`ファイルを作成します：
```php
use PHPUnit\Framework\TestCase;

// 数字を追加するCalculatorクラスがあると仮定します
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
テストを実行するには：
```bash
./vendor/bin/phpunit tests
```

#### サンプル出力：
```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:00.005, Memory: 6.00 MB

OK (1 test, 1 assertion)
```

### サードパーティライブラリ – Mockery
複雑なテスト、オブジェクトのモックを含む場合、Mockeryが人気の選択です。

```bash
composer require --dev mockery/mockery
```

#### PHPUnitとのMockeryの統合：
```php
use PHPUnit\Framework\TestCase;
use Mockery as m;

class ServiceTest extends TestCase
{
    public function tearDown(): void
    {
        m::close();
    }

    public function testServiceCallsExternalService()
    {
        $externalServiceMock = m::mock(ExternalService::class);
        $externalServiceMock->shouldReceive('process')->once()->andReturn('mocked result');

        $service = new Service($externalServiceMock);
        $result = $service->execute();

        $this->assertEquals('mocked result', $result);
    }
}
```
実行するには、上記と同じPHPUnitコマンドを使用します。Mockeryを使用すると、表現力豊かで柔軟なモックオブジェクトを利用でき、アプリケーション内の複雑な相互作用のテストが容易になります。
