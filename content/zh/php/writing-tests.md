---
title:                "编写测试代码"
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (什么及为何？)
编写测试是开发流程中创建代码来检查软件功能的步骤。程序员这样做是为了确保软件按预期工作，减少未来错误和重构带来的风险。

## How to: (操作指南)
在PHP中，PHPUnit是最常用的测试框架。这里有一个简单的PHPUnit使用示例:

```PHP
<?php
use PHPUnit\Framework\TestCase;

// 一个简单的函数，用于测试
function add($a, $b) {
    return $a + $b;
}

// 测试类
class AddTest extends TestCase {
    public function testAdd() {
        $this->assertEquals(4, add(2, 2));
    }
}
?>
```

运行测试，输出如下:
```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:01.234, Memory: 20.00 MB
```

## Deep Dive (深入了解)
PHPUnit起源于2004年，基于xUnit架构。除了PHPUnit，其他测试框架如PHPSpec, Codeception也受欢迎。正确实现测试需要掌握测试驱动开发(TDD)的原则：先写测试，后写逻辑代码。

## See Also (另见)
- PHPUnit官网: [https://phpunit.de/](https://phpunit.de/)
- 测试驱动开发 (Test-Driven Development): [https://en.wikipedia.org/wiki/Test-driven_development](https://en.wikipedia.org/wiki/Test-driven_development)
- PHPSpec官网: [http://www.phpspec.net/](http://www.phpspec.net/)
- Codeception官网: [https://codeception.com/](https://codeception.com/)