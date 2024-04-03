---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:58.489122-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Swift \u901A\u8FC7\u5176 XCTest \u6846\
  \u67B6\u652F\u6301\u6D4B\u8BD5\uFF0C\u8BE5\u6846\u67B6\u96C6\u6210\u5728 Xcode \u4E2D\
  \u3002\u4F60\u53EF\u4EE5\u7F16\u5199\u5355\u5143\u6D4B\u8BD5\u6765\u9A8C\u8BC1\u4EE3\
  \u7801\u7684\u5404\u4E2A\u90E8\u5206\uFF0C\u4F8B\u5982\uFF0C\u4E00\u4E2A\u8BA1\u7B97\
  \u4E24\u4E2A\u6570\u5B57\u4E4B\u548C\u7684\u51FD\u6570\u3002"
lastmod: '2024-03-13T22:44:48.161664-06:00'
model: gpt-4-0125-preview
summary: "Swift \u901A\u8FC7\u5176 XCTest \u6846\u67B6\u652F\u6301\u6D4B\u8BD5\uFF0C\
  \u8BE5\u6846\u67B6\u96C6\u6210\u5728 Xcode \u4E2D\u3002\u4F60\u53EF\u4EE5\u7F16\u5199\
  \u5355\u5143\u6D4B\u8BD5\u6765\u9A8C\u8BC1\u4EE3\u7801\u7684\u5404\u4E2A\u90E8\u5206\
  \uFF0C\u4F8B\u5982\uFF0C\u4E00\u4E2A\u8BA1\u7B97\u4E24\u4E2A\u6570\u5B57\u4E4B\u548C\
  \u7684\u51FD\u6570."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 如何操作：
Swift 通过其 XCTest 框架支持测试，该框架集成在 Xcode 中。你可以编写单元测试来验证代码的各个部分，例如，一个计算两个数字之和的函数。

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "求和函数没有返回预期的值。")
    }
}
```

要运行此测试，你通常会在 Xcode 中按 Command-U。Xcode 测试导航器中的输出将告诉你测试是通过还是失败。

例如，一个成功的测试输出：
```
测试用例 '-[YourAppTests testSum]' 通过 (0.005 秒)。
```

对于更高级的测试场景，你可能会采用第三方库，例如 Quick/Nimble，它们提供更具表现力的语法来编写测试。

使用 Quick/Nimble，你可能会这样编写相同的测试：

```swift
// 将 Quick 和 Nimble 添加到你的 Swift 包管理器中，或使用 CocoaPods/Carthage 安装它们
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("计算器") {
            context("当计算数字之和时") {
                it("应该返回正确的和") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

运行此测试会在测试控制台或 CI/CD 工具的日志中给出类似的输出，指示测试是成功还是失败，为描述测试和期望提供更可读的格式。
