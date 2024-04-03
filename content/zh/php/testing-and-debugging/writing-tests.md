---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:23.357553-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A #."
lastmod: '2024-03-13T22:44:47.867656-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 如何操作：


### 原生 PHP – PHPUnit
一个在 PHP 中广泛使用的测试工具是 PHPUnit。通过 Composer 安装：
```bash
composer require --dev phpunit/phpunit ^9
```

#### 编写一个简单的测试：
在 `tests` 目录下创建一个 `CalculatorTest.php` 文件：
```php
use PHPUnit\Framework\TestCase;

// 假设你有一个加法的 Calculator 类
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
运行测试：
```bash
./vendor/bin/phpunit tests
```

#### 示例输出：
```
PHPUnit 9.5.10 由 Sebastian Bergmann 和贡献者编写。

.                                                                   1 / 1 (100%)

时间：00:00.005，内存：6.00 MB

OK (1 测试, 1 断言)
```

### 第三方库 – Mockery
对于包括模拟对象在内的复杂测试，Mockery 是一个受欢迎的选择。

```bash
composer require --dev mockery/mockery
```

#### 将 Mockery 与 PHPUnit 集成：
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
要运行，请使用与上面相同的 PHPUnit 命令。Mockery 允许表达和灵活的模拟对象，方便测试应用程序内的复杂交互。
