---
title:    "Swift: 문자열을 소문자로 변환하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것에 참여하는 이유는 단순합니다. 일반적으로 다른 문자열 작업을 수행할 때 필요할 수 있습니다.

## 하는 방법
```Swift
let str = "Hello World"
let lowercasedStr = str.lowercased()

print(lowercasedStr) // Output: hello world
```

위의 예제에서는 문자열 "Hello World"를 선언하고, `lowercased()` 메소드를 사용하여 해당 문자열을 소문자로 변환하고, 결과를 출력하는 방법을 보여줍니다.

## 심층 탐구
문자열을 소문자로 변환하는 방법은 여러 가지가 있습니다. 하지만 가장 간단하고 빠른 방법은 `lowercased()` 메소드를 사용하는 것입니다. 이 메소드는 문자열을 소문자로 변환하여 새로운 문자열을 반환합니다. 또 다른 방법으로는 `NSString` 클래스의 `lowercased()` 메소드를 사용하는 것도 있습니다. 하지만 이 방법은 `NSString` 객체에만 적용할 수 있기 때문에, 일반적으로 `lowercased()` 메소드를 사용하는 것이 좋습니다.

## 더 많은 정보
문자열을 소문자로 변환하는 것은 간단한 작업이지만, 실제로는 문자열을 조작하는 데 필수적입니다. 이 작업을 할 때는 대소문자를 구분하거나 비교하기 전에 먼저 소문자로 변환하는 것이 좋습니다.

## 관련 링크
- [Swift strings 영문 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Apple Developer 문자열 관련 영문 문서](https://developer.apple.com/documentation/swift/string)
- [Swift 문자열 리터럴 관련 영문 문서](https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html#grammar_string-literal)
- [Swift 문자 처리 관련 영문 문서](https://docs.swift.org/swift-book/LanguageGuide/Unicode.html#//apple_ref/doc/uid/TP40014097-CH32-ID917)