---
title:    "Swift: 테스트 작성하기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# 왜
테스트를 작성하는 것에 참여하는 이유는 코드를 테스트하여 버그를 찾고 예상치 못한 오류를 방지하기 위해서입니다.

## 어떻게
테스트 코드를 작성하는 방법은 간단합니다. 먼저 ```XCTAssert```를 사용하여 코드의 각 부분을 테스트하고 그 결과를 확인합니다. 예를 들어, 다음의 코드는 "Hello World"가 제대로 출력되는지 테스트하는 것입니다.

```Swift
func testPrintHelloWorld() {
    let greeting = "Hello World"
    XCTAssertEqual(greeting, "Hello World")
}
```

## 깊이 파고들기
테스트를 작성할 때, 가장 중요한 것은 모든 경우의 수를 고려하는 것입니다. 예를 들어, 함수를 테스트할 때, 모든 입력값에 대해 테스트를 작성하는 것이 좋습니다. 또한, 테스트 코드를 작성할 때는 가독성을 위해 ```XCTAssert```를 사용하는 것이 좋습니다. 아래의 예시 코드는 매개변수로 받는 두 숫자를 더한 결과를 반환하는 함수를 테스트하는 것입니다.

```Swift
func testAddition() {
    let result = addNumbers(5, 10)
    XCTAssertEqual(result, 15)
}
```

# 참고
- [https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [https://developer.apple.com/documentation/xctest](https://developer.apple.com/documentation/xctest)
- [https://www.appcoda.com.tw/unit-testing-swift/](https://www.appcoda.com.tw/unit-testing-swift/)