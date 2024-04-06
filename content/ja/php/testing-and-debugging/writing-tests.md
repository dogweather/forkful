---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:26.447635-07:00
description: "\u65B9\u6CD5\uFF1A PHP\u3067\u306E\u30C6\u30B9\u30C8\u306B\u5E83\u304F\
  \u4F7F\u7528\u3055\u308C\u3066\u3044\u308B\u30C4\u30FC\u30EB\u306FPHPUnit\u3067\u3059\
  \u3002Composer\u7D4C\u7531\u3067\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\
  \u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.251248-06:00'
model: gpt-4-0125-preview
summary: "PHP\u3067\u306E\u30C6\u30B9\u30C8\u306B\u5E83\u304F\u4F7F\u7528\u3055\u308C\
  \u3066\u3044\u308B\u30C4\u30FC\u30EB\u306FPHPUnit\u3067\u3059\u3002Composer\u7D4C\
  \u7531\u3067\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\uFF1A."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
