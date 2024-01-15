---
title:                "문자열을 소문자로 변환하기"
html_title:           "Swift: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것에 참여하는 이유는 프로그래밍에서 문자열을 비교할 때 대소문자를 구분하지 않는 경우가 많기 때문입니다.

## 어떻게
```Swift
let str = "Hello, world!"
let lowercasedStr = str.lowercased()
print(lowercasedStr)
// Output: hello, world!
```
위의 예제 코드에서 `lowercased()` 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다. 이렇게 하면 대문자를 소문자로 변환하여 비교할 때 유용하게 사용할 수 있습니다. 

## 딥 다이브
Swift의 `String` 타입은 기본적으로 대소문자를 구분합니다. 그러나 `lowercased()` 함수를 사용하면 문자열을 소문자로 변환하여 대소문자를 구분하지 않는 비교를 할 수 있습니다. 또한 `localizedLowercase`와 같은 다른 함수들도 있으며 이들은 다른 언어에서도 작동하는 유용한 기능을 제공합니다. 또한 `caseInsensitiveCompare()` 함수를 사용하여 두 문자열을 비교할 때 대소문자를 구분하지 않을 수도 있습니다.

## See Also
- [Swift Documentation: String](https://developer.apple.com/documentation/swift/string)
- [Hacking with Swift: How to lowercase strings in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-lowercase-strings-in-swift)