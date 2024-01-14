---
title:                "Swift: 문자열 연결"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

스트링을 연결하는 것의 이점은 가독성과 효율성을 높여줍니다.

## 하는 방법

```Swift
// 기본적인 스트링 연결
let greeting = "안녕"
let name = "제이슨"
let fullGreeting = greeting + ", " + name
print(fullGreeting)
// 출력: 안녕, 제이슨
```

```Swift
// 여러 스트링을 한 번에 연결
let first = "한국"
let second = "어학당"
let third = "에서 공부하다."
let location = first + second + third
print(location)
// 출력: 한국어학당에서 공부하다.
```

```Swift
// 변수나 상수와 스트링을 동시에 연결
let age = 35
let message = "내 나이는"
let description = message + String(age) + "살 입니다."
print(description)
// 출력: 내 나이는 35살 입니다.
```

## 딥 다이브

스트링 연결 시 사용하는 `+` 연산자는 내부적으로 `append()` 함수를 호출합니다. 또한, `append()` 함수를 여러 번 연속해서 호출하는 것보다 `+` 연산자를 사용하는 것이 성능이 더 좋습니다.

## 참고

- [Apple Developer Documentation](https://developer.apple.com/documentation/swift/string/1688831-append)
- [Swift Programming Language Guide](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)