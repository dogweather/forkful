---
title:                "문자열 소문자로 변환하기"
html_title:           "Swift: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?:
문자열을 소문자로 변환하는 것은 간단하게 말하면 대문자를 소문자로 바꾸는 것이며, 프로그래머가 그렇게 하는 이유는 텍스트 데이터를 일관성 있게 처리하기 위해서입니다.

## 하는 방법:
```Swift
let str = "HELLO WORLD"
let lowerCaseStr = str.lowercased()
print(lowerCaseStr)
```
출력 결과:
hello world

## 깊이 들어가기:
- 문자열 변환은 대소문자를 구분하지 않는 언어에서 특히 중요합니다.
- 문자열의 대소문자를 구분하지 않는 다른 방법으로는 대소문자 대조(compare) 메소드를 사용할 수 있습니다.
- 소문자 변환은 문자 하나하나에 접근하여 대문자를 소문자로 바꾸는 알고리즘을 이용합니다.

## 관련 자료:
- [Swift 문자열 공식 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [String 클래스 참고 가이드](https://developer.apple.com/documentation/swift/string)
- [문자열 변환 방법 문서](https://stackoverflow.com/questions/6954069/how-to-convert-string-to-lowercase-in-swift)