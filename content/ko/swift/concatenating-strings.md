---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이고 왜하나요?

문자열 연결(string concatenation)은 두 개 이상의 문자열을 한 개로 합치는 프로그래밍 기법입니다. 코드의 가독성을 높이고 자료 구조 또는 출력 메세지를 복잡하지 않게 만드는 데 유용합니다. 

## 아래와 같이 구현합니다:

Swift에서 문자열 연결을 하는 가장 간단한 방법은 `+=` 연산자를 사용하는 것입니다.

```Swift
var str1 = "Hello, "
str1 += "World!"
print(str1)  // 출력: Hello, World!
``` 

또, `+` 연산자를 통해 두 개의 문자열을 한 번에 합칠 수도 있습니다.

```Swift
let str2 = "Hello, " + "World!"
print(str2)  // 출력: Hello, World!
```

## Deep Dive 

문자열 연결은 프로그래밍의 초기부터 존재해왔습니다. Swift에서의 문자열 연결은 `String` 클래스가 제공하는 `+=`와 `+` 연산자를 이용해서 훨씬 쉽게 구현할 수 있습니다.

Swift 이외에도 많은 프로그래밍 언어들은 이러한 기능을 지원하지만 그 방식은 언어마다 다릅니다. Python의 경우 문자열 포맷팅, Java의 경우 문자열 빌더 등의 방식을 택하였습니다.

`+=`와 `+` 연산자 외에도 Swift에서 문자열을 합치기 위해 `append()` 메소드를 사용할 수 있습니다. 이 메소드는 기존 문자열 뒤에 새 문자열을 이어 붙입니다.

```Swift
var str3 = "Hello, "
str3.append("World!")
print(str3)  // 출력: Hello, World!
```
## 참고 자료 

1. Swift 공식 문서의 문자열 및 문자[링크](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. Swift 문자열 연결에 대한 Stack Overflow 토론 [링크](https://stackoverflow.com/questions/24051314/whats-the-string-concatenation-operator-in-swift)
3. Swift 튜토리얼 [링크](https://swift.org/documentation/#the-swift-programming-language)