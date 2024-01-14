---
title:    "Swift: 문자열의 첫 글자를 대문자로 만들기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

한글로 쓰인 문자열의 첫 글자를 대문자로 바꾸는 것이 유용한 경우가 많습니다. 예를 들어, 사용자가 이름을 입력할 때 대문자로 시작하게 하기 위해 이 기능을 사용할 수 있습니다.

## 방법

```Swift
let name = "김철수"
print(name.capitalized)

// Output: 김철수
```

이렇게 간단한 코드 한 줄로 문자열의 첫 글자를 대문자로 바꿀 수 있습니다. 이 기능은 문자열의 나머지 부분은 변경하지 않기 때문에 유용하게 사용할 수 있습니다. 만약 전체 문자열을 대문자로 바꾸고 싶다면 `uppercased()` 메서드를 사용할 수 있습니다.

```Swift
let name = "김철수"
print(name.uppercased())

// Output: 김철수
```

이처럼 `capitalized` 메서드와 `uppercased` 메서드는 문자열을 변경하지 않고 새로운 값을 리턴하기 때문에 값이 바뀌지 않는다는 점을 주의해야 합니다. 그래서 적용한 결과를 변수에 저장하거나 출력할 때만 적용됩니다.

## 깊이 파헤치기

두 메서드의 차이점은 어떤 문자열이든 대문자로 변환하는 것인지에 있습니다. `capitalized` 메서드는 문자열의 첫 글자만 대문자로 변환하지만 `uppercased` 메서드는 모든 글자를 대문자로 변환합니다. 따라서 사용하는 상황에 따라 적절한 메서드를 선택해야 합니다.

## 관련 항목

- [String에 대한 Apple 공식 문서](https://developer.apple.com/documentation/foundation/nsstring/1414082-uppercase)
- [Swift에서 문자열 다루기](https://juyounglee.com/2019/07/16/swift_string/)
- [온라인 Swift 플레이그라운드](https://swift.sandbox.bluemix.net/#/repl)