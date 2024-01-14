---
title:                "Swift: 문자열 추출하기"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

Swift 프로그래밍을 하다 보면 문자열에서 원하는 부분만 가져와야 할 때가 있습니다. 그럴 때 문자열의 일부분을 추출하는 것이 중요한데요, 이 글에서는 문자열에서 원하는 부분을 추출하는 방법을 소개하도록 하겠습니다.

## 추출하는 방법

문자열에서 일부분을 추출하는 방법은 아주 간단합니다. 우선, 우리가 추출하려는 문자열이 변수에 저장되어 있어야 합니다. 그리고 그 변수를 사용하여 `substring` 메소드를 호출하면 됩니다.

```Swift
let sentence = "저는 스위프트를 배우고 있습니다."
let substring = sentence.substring(from: 3)
print(substring)
```

위 코드의 실행 결과는 다음과 같습니다.

```
스위프트를 배우고 있습니다.
```

위의 예시에서는 문자열의 가장 첫 글자부터 세 번째 글자 이후의 문자열을 추출했습니다. 만약 다른 범위의 문자열을 추출하려면 `substring` 메소드를 적절하게 호출하면 됩니다.

## 더 깊게 알아보기

`substring` 메소드에는 몇 가지 다른 형태가 있습니다. 예를 들어, `substring(to:)` 메소드를 사용하면 문자열의 처음부터 원하는 인덱스까지의 문자열을 추출할 수 있고, `substring(with:)` 메소드를 사용하면 문자열의 특정 범위의 문자열을 추출할 수 있습니다. 또한, `String` 타입의 `range(of:)` 메소드를 사용하여 원하는 문자열의 범위를 가져온 다음, 그 범위를 사용하여 `substring(with:)` 메소드를 호출하여 원하는 문자열을 추출할 수도 있습니다.

## 참고

[Apple Developer Documentation - String](https://developer.apple.com/documentation/swift/string)

[Swift Programming Language Guide - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

[Swift by Sundell - Substrings](https://www.swiftbysundell.com/basics/substrings/)