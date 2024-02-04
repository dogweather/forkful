---
title:                "编写测试"
date:                  2024-02-03T19:31:58.489122-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用 Swift 编写测试涉及创建并执行代码，以验证应用程序中其他代码单元的正确性。程序员之所以这样做，是为了确保可靠性，早期发现开发周期中的错误，并促进未来的代码重构而不引发意外后果。

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
