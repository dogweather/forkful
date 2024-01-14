---
title:                "Swift: 문자열 소문자로 변환하기"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜

문자열을 대문자에서 소문자로 변환하는 것에 관심이 있다면, 언어의 대소문자 구별 여부나 비교를 위해 필요할 수 있습니다.

## 하우 투

```swift
let str = "Hello, World!"
print(str.lowercased())
// Output: hello, world!
```

```swift
let str = "1,2,3,4"
print(str.lowercased())
// Output: 1,2,3,4
```

비교적 간단한 코드이지만, 문자열이 대문자로 시작하거나 숫자만 포함된 경우에도 소문자로 변환할 수 있습니다.

## 딥 다이브

Swift에서 ```lowercased()``` 메소드는 문자열을 모두 소문자로 변환하기 위해 사용됩니다. 이 메소드는 문자열에서 모든 문자를 검색하고 소문자로 변환한 새로운 문자열을 반환합니다. 이 과정에서, 원래 문자열은 변경되지 않습니다.

하지만, 문자열이 아닌 다른 타입의 값에서 이 메소드를 사용할 수도 있습니다. 예를 들어, 다음과 같이 사용할 수 있습니다.

```swift
let number = 123
print(String(number).lowercased())
// Output: 123
```

또 다른 유용한 기능은 ```uppercased()``` 메소드입니다. 이를 사용하면 소문자를 대문자로 변환할 수 있습니다. 이를 이용하면 대소문자 구분 없이 문자열을 비교할 수 있습니다.

## 참고

[Apple Developer Documentation - lowercased()](https://developer.apple.com/documentation/swift/string/2894120-lowercased)

[Swift.org - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID313)

[Swift by Sundell - Working with strings in Swift](https://www.swiftbysundell.com/basics/strings/)
# 참조

[Apple Developer Documentation - lowercased()](https://developer.apple.com/documentation/swift/string/2894120-lowercased)

[Swift.org - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID313)

[Swift by Sundell - Working with strings in Swift](https://www.swiftbysundell.com/basics/strings/)