---
title:    "Swift: 문자열의 길이를 찾는 방법"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜: 문자열의 길이를 찾는 데 참여하는 과정의 의미
문자열의 길이를 찾는 것은 프로그래밍에 있어서 중요한 단계입니다. 예를 들어, 사용자의 입력값을 처리하기 위해 문자열을 사용하는 경우, 문자열의 길이를 확인해야 합니다. 이를 통해 사용자가 올바른 형식의 입력값을 제공했는지 확인할 수 있습니다.

## 어떻게: Swift로 문자열의 길이를 확인하는 방법
먼저 문자열을 변수에 할당하고, 이를 이용해 `count` 메소드를 호출합니다. 아래는 간단한 코드 예시입니다.

```Swift
let str = "Hello, world!"
print(str.count) // 결과: 13
```

## 깊게 들어가기: 문자열의 길이를 찾는 과정에 대한 더 깊은 정보
Swift에서 문자열을 다루는 데에는 `String`이라는 자료형을 사용합니다. 이 자료형은 여러 문자들로 이루어진 값의 집합이며, 각 문자는 `Character` 자료형으로 이루어져 있습니다. 따라서 `String` 자료형에서 제공하는 `count` 메소드를 사용하면 문자열의 길이를 쉽게 확인할 수 있습니다.

# 참고 자료
* [Swift 문서 - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
* [Swift 기본 문법 강좌 - 문자열 다루기](https://medium.com/@kimyoungwon/3-26-swift-%EC%95%88%EC%A0%84-%EA%B8%B0%EB%B3%B8%EC%97%90%EC%84%9C-%EB%AC%B8%EC%9E%90%EC%97%B4-%EB%8B%A4%EB%A3%A8%EA%B8%B0-3e3885294fa1)
* [Swift 공식 레퍼런스 - String.count](https://developer.apple.com/documentation/swift/string/2427941-count)

## 참조
[Swift 문서 - 정수타입(Integers)](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID335)