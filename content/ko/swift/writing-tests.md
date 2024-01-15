---
title:                "테스트 작성하기"
html_title:           "Swift: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-tests.md"
---

{{< edit_this_page >}}

## 왜?

최신 버전의 Swift를 이용하여 테스트를 작성하는 이유는 코드의 안정성과 신뢰성을 높이기 위해서입니다.

## 작성하는 방법

```Swift
// 예시: 합 구하기 함수 
func getSum(numbers: [Int]) -> Int {
  var sum = 0
  for number in numbers {
    sum += number
  }
  return sum
}
```

테스트를 작성하는 첫 번째 단계는 간단한 작은 함수를 만드는 것입니다. 예를 들어, 위의 예시에서는 숫자 배열을 받아 그 합을 반환하는 함수를 만들었습니다. 이 함수를 실행해보면 제대로 작동하는 것을 확인할 수 있습니다. 그러나 이 함수가 항상 정확하게 작동하는지 확신할 수 없습니다. 이 때, 테스트를 작성할 차례입니다.

```Swift
// 예시: 테스트 코드
let numbers = [2, 4, 6]
let result = getSum(numbers: numbers)
assert(result == 12, "getSum 함수가 정확한 합을 반환하지 않습니다.")
```

위의 코드는 `assert()` 함수를 이용하여 우리가 작성한 `getSum()` 함수가 제대로 동작하는지 테스트하는 코드입니다. `assert()` 함수는 첫 번째 파라미터로 조건을, 두 번째 파라미터로 조건이 참이지 않을 때 출력될 메시지를 전달합니다. 결과를 확인해보면 조건이 참이지 않아서 메시지가 출력되지 않는 것을 볼 수 있습니다. 즉, 우리가 작성한 함수가 제대로 작동하고 있음을 알 수 있습니다.

## 더 깊이 들어가기

위의 예시는 단순한 함수를 테스트하는 것이지만, 실제로는 보다 복잡한 코드들을 테스트해야 합니다. 이 때, `XCTest` 프레임워크를 사용하면 효과적으로 테스트 코드를 작성할 수 있습니다. `XCTest` 프레임워크는 테스트 케이스를 하나의 클래스로 묶어서 관리할 수 있습니다. 테스트 케이스는 여러 개의 테스트 메소드로 구성되어 있고, 각각의 메소드는 `XCTAssert` 함수를 이용하여 테스트를 진행합니다. 이렇게 작성한 테스트 코드는 `XCTest`에서 제공하는 간단한 명령어 하나로 실행할 수 있어서 편리합니다.

## 관련 링크 (See Also)

- [Apple 공식 문서 - Testing with Xcode](https://developer.apple.com/documentation/xctest)

- [Raywenderlich - iOS TDD by Example: Part 1 - Introduction](https://www.raywenderlich.com/150073/ios-test-driven-development-tutorial-part-1-introduction)

- [Realm Academy - TDD(Test Driven Development)란 무엇인가요?](https://academy.realm.io/kr/posts/qa-with-vincent-pradeilles-ios-test-driven-development/)