---
title:                "테스트 작성하기"
date:                  2024-02-03T19:32:05.546152-07:00
model:                 gpt-4-0125-preview
simple_title:         "테스트 작성하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Swift로 테스트 작성은 애플리케이션의 다른 코드 단위의 정확성을 검증하는 코드를 생성하고 실행하는 것을 포함합니다. 프로그래머들은 신뢰성을 보장하고, 개발 주기 초기에 버그를 감지하며, 의도하지 않은 결과 없이 미래 코드 리팩토링을 용이하게 하기 위해 이 작업을 합니다.

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
