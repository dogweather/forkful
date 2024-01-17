---
title:                "문자열 대문자화하기"
html_title:           "Swift: 문자열 대문자화하기"
simple_title:         "문자열 대문자화하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 대문자로 바꾸기는 무엇인가요? 프로그래머들이 왜 이 작업을 하는 걸까요?

## 방법:

```Swift
let sampleString = "hello, world!"
let capitalizedString = sampleString.uppercased()
print(capitalizedString) // Prints "HELLO, WORLD!"
```

```Swift
var sampleString = "hello, world!"
sampleString.capitalizeFirstLetter()
print(sampleString) // Prints "Hello, world!"
```

## 깊이 들어가보기:

(1) 역사적 맥락: 문자열 대문자로 바꾸기는 문자열을 다루는 프로그래밍에서 오랜 영향을 미쳤습니다.

(2) 대안: 문자열 대문자로 바꾸기는 다양한 방법으로 구현할 수 있습니다. 위 코드는 가장 일반적인 방법이지만, 다른 언어에서도 지원하거나 기존 라이브러리에서 제공할 수도 있습니다.

(3) 구현 세부 사항: ```uppercased()``` 메소드는 문자열의 모든 문자를 대문자로 바꿀 뿐만 아니라 유니코드 기반으로 구현되어 여러 언어를 지원합니다.

## 관련 자료:

- [Swift 문자열 대문자로 바꾸기 메소드](https://developer.apple.com/documentation/swift/string#2854681)
- [Swift 문자열 처리 공식 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)