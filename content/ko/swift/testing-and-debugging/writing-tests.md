---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:05.546152-07:00
description: "\uBC29\uBC95: Swift\uB294 XCTest \uD504\uB808\uC784\uC6CC\uD06C\uB97C\
  \ \uD1B5\uD55C \uD14C\uC2A4\uD2B8\uB97C \uC9C0\uC6D0\uD558\uBA70, \uC774\uB294 Xcode\uC5D0\
  \ \uD1B5\uD569\uB418\uC5B4 \uC788\uC2B5\uB2C8\uB2E4. \uC608\uB97C \uB4E4\uC5B4,\
  \ \uB450 \uC218\uC758 \uD569\uC744 \uACC4\uC0B0\uD558\uB294 \uD568\uC218\uC640 \uAC19\
  \uC740 \uCF54\uB4DC\uC758 \uAC1C\uBCC4 \uBD80\uBD84\uC744 \uAC80\uC99D\uD558\uAE30\
  \ \uC704\uD55C \uB2E8\uC704 \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.736697-06:00'
model: gpt-4-0125-preview
summary: "Swift\uB294 XCTest \uD504\uB808\uC784\uC6CC\uD06C\uB97C \uD1B5\uD55C \uD14C\
  \uC2A4\uD2B8\uB97C \uC9C0\uC6D0\uD558\uBA70, \uC774\uB294 Xcode\uC5D0 \uD1B5\uD569\
  \uB418\uC5B4 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
Swift는 XCTest 프레임워크를 통한 테스트를 지원하며, 이는 Xcode에 통합되어 있습니다. 예를 들어, 두 수의 합을 계산하는 함수와 같은 코드의 개별 부분을 검증하기 위한 단위 테스트를 작성할 수 있습니다.

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "합수 함수가 예상한 값을 반환하지 않았습니다.")
    }
}
```

이 테스트를 실행하려면, 일반적으로 Xcode에서 Command-U를 누릅니다. Xcode 테스트 네비게이터의 출력은 테스트가 통과했는지 실패했는지를 알려줄 것입니다.

예를 들어, 성공적인 테스트 출력:
```
Test Case '-[YourAppTests testSum]' passed (0.005 seconds).
```

보다 고급 테스트 시나리오의 경우, Quick/Nimble과 같은 제3자 라이브러리를 채택할 수 있으며, 이는 테스트 작성에 있어 더 표현적인 문법을 제공합니다.

Quick/Nimble을 사용하면 다음과 같이 동일한 테스트를 작성할 수 있습니다:

```swift
// Swift 패키지 매니저에 Quick과 Nimble을 추가하거나 CocoaPods/Carthage를 사용하여 설치하세요
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("계산기") {
            context("숫자를 더할 때") {
                it("정확한 합을 반환해야 합니다") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

이 테스트를 실행하면 테스트 콘솔이나 CI/CD 도구의 로그에서 비슷한 출력을 보여줄 것이며, 테스트와 기대 사항을 설명하는 데 있어 더 읽기 쉬운 형식으로 테스트가 성공했는지 또는 실패했는지를 나타냅니다.
