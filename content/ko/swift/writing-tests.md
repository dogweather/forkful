---
title:    "Swift: 테스트 작성하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 테스트 코딩을 해야 하는가?

프로그래밍은 많은 사용자들에게 좋은 경험을 제공하기 위해서 정확하게 작동하는 것이 중요합니다. 하지만 수많은 코드들을 짜는 것 자체로는 정확성을 보장하기 어렵습니다. 이때 테스트 코딩을 통해 코드의 정확성을 검증할 수 있습니다.

## 어떻게 테스트 코딩을 해야 할까

먼저 XCTest 프레임워크를 사용하여 테스트 코드를 작성해야 합니다. 아래는 간단한 코드 예시입니다.

```
Swift func addTwoNumbers(a: Int, b:Int) -> Int {
    return a + b
}

class CalculatorTests: XCTestCase {
    func testAddTwoNumbers() {
        XCTAssertEqual(addTwoNumbers(a: 1, b: 2), 3)
    }
}
```

위와 같이 테스트 코드를 작성한 뒤, Command+U를 눌러 테스트를 실행하고 결과를 확인할 수 있습니다. 위 코드의 경우, 결과는 'Passed'가 될 것입니다.

## 딥 다이브

테스트 코딩을 하기 위해서는 코드의 단위가 중요합니다. 즉, 기능별로 테스트를 나눠서 작성해야 합니다. 또한 테스트가 코드의 부작용을 최소화하는데 도움이 되므로 실제 앱이나 서비스를 출시하기 전에는 가능한 많은 테스트를 작성하는 것이 좋습니다.

## 참고 자료

- [XCTest 프레임워크 문서](https://developer.apple.com/documentation/xctest)

- [애플 개발자 블로그: Writing Effective Tests](https://developer.apple.com/swift/blog/?id=10)

- [TDD vs BDD: 단위 테스트와 시나리오 테스트의 차이](https://www.raywenderlich.com/96657/unit-testing-tdd-vs-bdd)

# 관련 링크

- [스위프트 프로그래밍 공식 문서](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html)

- [애플 개발자 포럼](https://forums.developer.apple.com/community/swift)

- [스택 오버플로우: Swift 질문 및 답변 모음](https://stackoverflow.com/questions/tagged/swift)