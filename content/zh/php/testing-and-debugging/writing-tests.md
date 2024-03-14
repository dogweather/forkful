---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:23.357553-07:00
description: "\u7F16\u7A0B\u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\u548C\
  \u8FD0\u884C\u811A\u672C\uFF0C\u4EE5\u9A8C\u8BC1\u4EE3\u7801\u5728\u5404\u79CD\u6761\
  \u4EF6\u4E0B\u7684\u8868\u73B0\u662F\u5426\u7B26\u5408\u9884\u671F\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u786E\u4FDD\u8D28\
  \u91CF\u3001\u9632\u6B62\u56DE\u5F52\u548C\u4FC3\u8FDB\u5B89\u5168\u91CD\u6784\uFF0C\
  \u8FD9\u5BF9\u4E8E\u7EF4\u62A4\u4E00\u4E2A\u5065\u5EB7\u3001\u53EF\u6269\u5C55\u4E14\
  \u65E0\u9519\u8BEF\u7684\u4EE3\u7801\u5E93\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:47.867656-06:00'
model: gpt-4-0125-preview
summary: "\u7F16\u7A0B\u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\u548C\
  \u8FD0\u884C\u811A\u672C\uFF0C\u4EE5\u9A8C\u8BC1\u4EE3\u7801\u5728\u5404\u79CD\u6761\
  \u4EF6\u4E0B\u7684\u8868\u73B0\u662F\u5426\u7B26\u5408\u9884\u671F\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u786E\u4FDD\u8D28\
  \u91CF\u3001\u9632\u6B62\u56DE\u5F52\u548C\u4FC3\u8FDB\u5B89\u5168\u91CD\u6784\uFF0C\
  \u8FD9\u5BF9\u4E8E\u7EF4\u62A4\u4E00\u4E2A\u5065\u5EB7\u3001\u53EF\u6269\u5C55\u4E14\
  \u65E0\u9519\u8BEF\u7684\u4EE3\u7801\u5E93\u81F3\u5173\u91CD\u8981\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
---

{{< edit_this_page >}}

## 什么与为什么？
编程中编写测试涉及创建和运行脚本，以验证代码在各种条件下的表现是否符合预期。程序员之所以这样做，是为了确保质量、防止回归和促进安全重构，这对于维护一个健康、可扩展且无错误的代码库至关重要。

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
