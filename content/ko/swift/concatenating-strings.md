---
title:                "문자열 연결하기"
html_title:           "Swift: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜?
문자열을 연결하는 것에 참여할 이유는 무엇일까요? Swift에서는 문자열을 쉽게 연결할 수 있는 기능이 있어 코드를 간단하게 작성할 수 있기 때문입니다.

## 하는 방법
```Swift 
let firstName = "John"
let lastName = "Smith"
let fullName = "\(firstName) \(lastName)"
print(fullName)
```
위 코드에서는 `firstName`과 `lastName` 변수를 선언하고 `\(firstName) \(lastName)`을 이용하여 두 변수의 값을 합쳐 `fullName` 변수에 저장합니다. 그리고 `print` 함수를 사용하여 `fullName`을 출력합니다. 결과는 "John Smith"가 됩니다.

## 깊이 파고들기
Swift에서는 문자열을 연결하기 위해 `+` 연산자나 `String` 클래스의 `append` 메서드를 사용할 수 있습니다. 하지만 `String interpolation` 문법을 이용하면 변수를 더 간결하게 연결할 수 있습니다. 또한 문자열뿐만 아니라 다른 타입과도 함께 사용할 수 있습니다.

## 참고 자료
[Swift 공식 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)