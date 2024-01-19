---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가? 
스트링 인터폴레이션은 변수, 상수, 표현식을 문자열에 포함하는 방법을 뜻합니다. 이를 통해 프로그래머는 동적으로 문자열을 구성하고 표현할 수 있습니다. 

## 사용방법:
Swift에서 문자열 인터폴레이션을 사용하는 방법은 간단합니다. `\()`를 사용하여 문자열에 직접 값을 삽입합니다. 코드 예제와 결과를 확인해 보겠습니다.

```Swift
let name = "Jay"
let greeting = "안녕하세요, \(name)님!"
print(greeting)
```
위의 코드를 실행시키면, 결과는 다음과 같이 나옵니다:

```
안녕하세요, Jay님!
```

## 깊게 들어가서 숙지하는 정보: 
스트링 인터폴레이션은 Swift 특징인 강력한 기능입니다. 이 기능은 기존의 C 또는 Objective-C와 같은 언어에서는 printf나 stringWithFormat:와 같은 함수를 사용해야 했던 것을 대체하였습니다.

대안으로, 문자열 결합은 더 간단한 경우에 사용될 수 있지만, 문자열 인터폴레이션은 더 복잡한 설정과 가독성을 향상시킵니다.

Swift 내부에는 `String Interpolation`이 어떻게 구현되었는지 살펴보면, `String Interpolation`은 `StringInterpolation` 프로토콜을 채택하여 작동하며, 이는 문자열을 구성하는 동안 값을 효율적으로 추가하는 메서드를 제공합니다.

## 참고 자료:
- Apple 공식 문서: [String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#//apple_ref/doc/uid/TP40014097-CH7-ID285)
- Swift by Sundell: [String Interpolation in Swift](https://www.swiftbysundell.com/posts/string-interpolation-in-swift)