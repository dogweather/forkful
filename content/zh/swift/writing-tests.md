---
title:                "编写测试代码"
date:                  2024-01-19
simple_title:         "编写测试代码"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
编写测试是验证代码按预期工作的过程。程序员这么做是为了确保质量，提前发现问题，节省未来修改成本。

## How to:
在Swift中，你可以使用XCTest框架来写单元测试。下面展示了如何测试一个简单的加法函数：

```Swift
import XCTest

class MathTests: XCTestCase {

    func testAddition() {
        let sum = addNumbers(a: 2, b: 3)
        XCTAssertEqual(sum, 5, "加法函数返回的结果不正确")
    }
    
    func addNumbers(a: Int, b: Int) -> Int {
        return a + b
    }
}

// 测试运行后的输出示例：
// Test Suite 'All tests' passed at 2023-01-01 12:00:00.
// Executed 1 test, with 0 failures (0 unexpected) in 0.005 (0.006) seconds
```

## Deep Dive
最早的测试框架SUnit由Kent Beck在Smalltalk语言中开发。现在Swift常用的XCTest是Xcode自带的测试框架，运用了SUnit的很多原理。当然，除了XCTest，你也可以选择其他库，比如Quick和Nimble，它们提供了不同风格的测试方法。实现细节上，编写测试代码时应确保测试独立且可重复，目的是模拟各种可能的使用场景。

## See Also
- [苹果官方文档—测试你的应用](https://developer.apple.com/documentation/xctest)
- [Ray Wenderlich关于Swift单元测试的教程](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
- [Quick GitHub页面](https://github.com/Quick/Quick)
