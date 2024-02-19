---
aliases:
- /zh/swift/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:58.489122-07:00
description: "\u4F7F\u7528 Swift \u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\u5E76\
  \u6267\u884C\u4EE3\u7801\uFF0C\u4EE5\u9A8C\u8BC1\u5E94\u7528\u7A0B\u5E8F\u4E2D\u5176\
  \u4ED6\u4EE3\u7801\u5355\u5143\u7684\u6B63\u786E\u6027\u3002\u7A0B\u5E8F\u5458\u4E4B\
  \u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u786E\u4FDD\u53EF\u9760\u6027\
  \uFF0C\u65E9\u671F\u53D1\u73B0\u5F00\u53D1\u5468\u671F\u4E2D\u7684\u9519\u8BEF\uFF0C\
  \u5E76\u4FC3\u8FDB\u672A\u6765\u7684\u4EE3\u7801\u91CD\u6784\u800C\u4E0D\u5F15\u53D1\
  \u610F\u5916\u540E\u679C\u3002"
lastmod: 2024-02-18 23:08:59.445346
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 Swift \u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\u5E76\
  \u6267\u884C\u4EE3\u7801\uFF0C\u4EE5\u9A8C\u8BC1\u5E94\u7528\u7A0B\u5E8F\u4E2D\u5176\
  \u4ED6\u4EE3\u7801\u5355\u5143\u7684\u6B63\u786E\u6027\u3002\u7A0B\u5E8F\u5458\u4E4B\
  \u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u786E\u4FDD\u53EF\u9760\u6027\
  \uFF0C\u65E9\u671F\u53D1\u73B0\u5F00\u53D1\u5468\u671F\u4E2D\u7684\u9519\u8BEF\uFF0C\
  \u5E76\u4FC3\u8FDB\u672A\u6765\u7684\u4EE3\u7801\u91CD\u6784\u800C\u4E0D\u5F15\u53D1\
  \u610F\u5916\u540E\u679C\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
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
