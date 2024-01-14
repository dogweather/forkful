---
title:                "Swift: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

사람들이 문자열에서 일부 문자열을 추출하는 것에 참여하기 위한 이유는 무엇일까요? 이 작업은 데이터 처리에서 매우 일반적이고 유용하며, 특정한 일을 수행하기 위해 문자열의 특정 부분만을 필요할 때 유용합니다. 또한 문자열에서 작업할 수있는 방법을 확장하여 더 복잡한 작업을 수행할 수도 있습니다.

## 추출하는 방법

```Swift
// 주어진 문자열
let str = "Hello World!"

// 첫 번째 문자부터 세 번째 문자까지 추출
let subString = str[0...2]

// 출력: "Hel"
print(subString)

// 마지막 문자만 추출
let lastChar = str.last

// 출력: "!"
print(lastChar)
```

### 내부 동작

위의 예시에서 보는 것처럼, 문자열에서 일부분을 추출하는 방법은 간단합니다. 대괄호를 사용하여 인덱스나 범위를 지정하여 사용할 수 있습니다. 또한 속성 `first`와 `last`를 사용하여 첫 번째와 마지막 문자를 추출할 수도 있습니다. 이러한 작업은 실제로 문자열의 `startIndex`와 `endIndex`를 사용하여 수행됩니다.

## 깊게 파고들기

문자열에서 일부분을 추출하는 것은 보통 간단한 작업으로 여겨집니다. 그러나 Swift의 내부 동작을 이해하고 이를 활용하여 더 복잡한 작업을 수행할 수 있다면 더욱 효율적인 프로그래밍을 할 수 있습니다. 예를들어, `substring(with: )` 메소드를 사용하면 중간에 문자열을 추출할 수 있으며, `contains()` 메소드를 사용하여 문자열 내에 특정 문자열이 포함되어 있는지 검사할 수도 있습니다.

## 더 찾아보기

[Apple 개발 문서](https://developer.apple.com/documentation/swift/string/2894561-substring)에서 `String` 클래스의 `substring(with: )`와 `contains()` 메소드에 대한 자세한 설명을 확인할 수 있습니다.

[Ray Wenderlich](https://www.raywenderlich.com/150073/swift-tutorial-extract-substrings-nsstring)에서는 `NSString`의 메소드를 사용하여 문자열에서 일부분을 추출하는 다양한 방법을 배울 수 있습니다.

[AppCoda](https://www.appcoda.com/swift-string/)에서는 `Character`와 `String`사이의 변환 및 문자열에서 작업을 수행하는 다른 유용한 메소드에 대해 알아볼 수 있습니다.

## 참고

[Swift 문자열 추출](https://ko.jeffreyfreeman.me/programming/2017/11/14/swift-string-extracting/), [Jeffreyfreeman](https://ko.jeffreyfreeman.me/) 블로그의 기사를 참조하였습니다.