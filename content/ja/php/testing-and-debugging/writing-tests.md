---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:26.447635-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.251248-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u30C6\
  \u30B9\u30C8\u306E\u8A18\u8FF0\u3068\u306F\u3001\u30B3\u30FC\u30C9\u304C\u3055\u307E\
  \u3056\u307E\u306A\u6761\u4EF6\u4E0B\u3067\u671F\u5F85\u901A\u308A\u306B\u52D5\u4F5C\
  \u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3059\u308B\u30B9\u30AF\u30EA\u30D7\u30C8\
  \u3092\u4F5C\u6210\u3057\u3066\u5B9F\u884C\u3059\u308B\u3053\u3068\u3092\u6307\u3057\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u54C1\u8CEA\u3092\
  \u4FDD\u8A3C\u3057\u3001\u30EA\u30B0\u30EC\u30C3\u30B7\u30E7\u30F3\u3092\u9632\u6B62\
  \u3057\u3001\u5B89\u5168\u306A\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\
  \u5BB9\u6613\u306B\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002\u3053\u308C\u306F\u3001\u5065\u5168\u3067\u30B9\u30B1\u30FC\u30E9\u30D6\
  \u30EB\u3067\u30D0\u30B0\u306E\u306A\u3044\u30B3\u30FC\u30C9\u30D9\u30FC\u30B9\u3092\
  \u7DAD\u6301\u3059\u308B\u305F\u3081\u306B\u91CD\u8981\u3067\u3059\u3002."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
