---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요할까?
문자열의 길이를 찾기는 문자열에 얼마나 많은 문자들이 포함되어 있는지를 결정하는 방법입니다. 이는 프로그래머들이 특정 문자열의 크기를 알아봄으로써 응용프로그램이 잘 작동하는지 이해하고, 효과적으로 데이터를 처리할 수 있게 돕습니다.

## 해보기:
볼 수 있는 코드이며 실행 결과를 보면:

```Swift
let str = "Hello, Swift!"
print("문자열의 길이는 \(str.count)입니다.")
```
출력결과는 다음과 같습니다:

```Swift
문자열의 길이는 13입니다.
```

## 깊게 알아보기:
Swift에서 문자열의 길이를 찾기는 여기에 언급한 `.count` 외에도 여러가지 방법으로 수행될 수 있습니다. 하지만 `.count`는 Swift 4에서 도입된 특징이며, 최신 버전에서 가장 효과적이고 빠른 방법으로 알려져 있습니다.

알TERNATIVE로, `NSString` 클래스의 `length` 속성을 사용할 수 있는데, 이는 Objective-C의 세상에서 아직도 널리 쓰이는 방법입니다. 하지만 Swift의 `.count`를 사용하는 것이 더 편리하고, 더욱 Swift스럽습니다.
```Swift
let str = NSString(string: "Hello, Swift!")
print("문자열의 길이는 \(str.length)입니다.")
```

## 참고 자료:
Swift의 공식 문서 : [Swift 문자열과 문자](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
StackOverflow: [Swift에서 문자열 길이 찾기](https://stackoverflow.com/questions/24092884/get-length-of-string-in-swift)