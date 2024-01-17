---
title:                "문자열 보간"
html_title:           "Swift: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇인가요? & 왜 하는 건가요?
문자열을 삽입하는 것은 문자열 내에서 변수 또는 상수 값을 삽입하는 프로그래밍 기법입니다. 변수나 상수 값을 문자열에 직접 적어두는 것보다 더 유연하고 편리하게 사용할 수 있어서 프로그래머들이 많이 사용합니다.

## 하는 방법:
```Swift
let name = "소희"
let age = 25
print("안녕하세요, 제 이름은 \(name)이고 나이는 \(age)살입니다.")
```
출력 결과: "안녕하세요, 제 이름은 소희이고 나이는 25살입니다."

## 깊이 파고들기:
1. 역사적인 맥락: 문자열 삽입이 가능한 기술은 오래된 역사를 가지고 있습니다. 애플의 Objective-C 언어에서는 %@나 %d를 사용해 변수를 문자열에 삽입할 수 있었고, 이를 보완하기 위해 Swift에서는 String interpolation을 도입했습니다.
2. 대체 가능한 방법: 문자열 삽입 기능을 대체할 수 있는 방법으로는 문자열 포매팅이 있습니다. 포매팅은 문자열의 특정 부분을 변수 값으로 바꿔주는 기능을 제공하지만 보다 복잡하고 가독성이 떨어지는 단점이 있습니다.
3. 구현 방법: String interpolation의 구현 방법은 컴파일러 내부에서 문자열을 파싱하고 변수 값을 삽입하는 과정을 거칩니다. 이를 통해 변수 값을 문자열에 쉽게 삽입할 수 있게 되었습니다.

## 관련 자료:
- [Swift 공식 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Hacking with Swift - String Interpolation](https://www.hackingwithswift.com/example-code/language/string-interpolation-in-swift)