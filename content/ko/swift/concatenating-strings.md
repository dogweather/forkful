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

## 무엇 & 왜?

문자열을 이어붙이는 것은 간단한 개념입니다. 이는 한 개 이상의 문자열을 결합하여 새로운 하나의 문자열로 만드는 것을 의미합니다. 프로그래머들은 이를 하는 이유는 문자열을 다룰 때 유용하기 때문입니다.

## 어떻게:

```Swift 
let greeting = "Hello"
let name = "John"
let message = greeting + " " + name

print(message)
// Output: "Hello John"
```

## 깊이 파고들기:

문자열을 연결하는 개념은 다양한 언어에서 모두 사용됩니다. 오래된 언어인 C에서는 `strcat()` 함수를 사용하거나 문자열 포맷팅을 사용합니다. 또한 Swift에서는 `+` 연산자를 사용하여 문자열을 이어붙일 수 있습니다. 다른 대안으로는 `String(StringInterpolation)`을 사용하는 방법이 있으며, 이를 통해 Swift 내부에서 문자열을 이어붙일 수 있습니다.

## 관련 자료:

- 문자열 다루기: [Swift Programming Language Guide](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- `StringInterpolation`에 대한 문서: [Apple Developer Documentation](https://developer.apple.com/documentation/swift/stringinterpolation)