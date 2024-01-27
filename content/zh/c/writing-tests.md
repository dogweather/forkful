---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
编写测试是创建用于检验代码正确性的程序过程。程序员这样做是为了确保软件按预期工作，降低未来出错的风险。

## How to: (怎么做：)
使用C语言的assert函数进行基本测试：
```C
#include <assert.h>

void testAddition() {
    int sum = 2 + 2;
    assert(sum == 4); // Passes if sum is 4
}

int main() {
    testAddition();
    printf("Test passed!\n");
    return 0;
}
```
输出为：
```
Test passed!
```
若`assert`条款失败，程序将终止，并显示错误信息。

## Deep Dive (深入探索)
编写测试的历史可以追溯到软件开发的早期。历史上，使用简单方法检测函数的行为，但现代更偏好采用框架如CUnit等。借助这些框架，可以组织和执行测试套件，提供详细的测试报告。更先进的实践，如TDD（测试驱动开发），首先编写测试，然后才是满足测试需求的代码。

## See Also (另请参阅)
- CUnit官方网站：http://cunit.sourceforge.net/
- 测试驱动开发（TDD）入门：https://en.wikipedia.org/wiki/Test-driven_development
- ANSI C标准（最新C语言标准）简介：https://en.wikipedia.org/wiki/ANSI_C
