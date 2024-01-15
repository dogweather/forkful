---
title:                "문자열의 길이 찾기"
html_title:           "Swift: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것은 프로그래밍에서 매우 흔한 작업입니다. 이를 통해 문자열을 다루고 처리하는 데 필요한 정보를 얻을 수 있기 때문에 많은 프로그래머가 사용합니다.

## 방법

```Swift
// 문자열의 길이를 찾는 방법
let str = "Hello, world!"
print(str.count)
// Output: 13
```

Swift에서는 문자열의 길이를 찾는 `count` 메서드를 제공합니다. 이를 사용하면 문자열 내에 있는 문자의 개수를 반환할 수 있습니다.

```Swift
// 빈 문자열의 길이
let emptyStr = ""
print(emptyStr.count)
// Output: 0
``` 

빈 문자열의 경우 `count` 메서드를 사용하면 0을 반환합니다. 따라서 문자열이 있는지 없는지를 확인할 수 있습니다.

## 깊이 파헤치기

문자열의 길이를 찾는 방법은 실제로 매우 간단합니다. Swift에서는 문자열 내의 모든 문자를 배열로 저장하여 그 배열의 개수를 세는 방식으로 길이를 계산합니다. 또한 문자열 내에 있는 유니코드 문자를 모두 고려하여 정확한 길이를 계산합니다. 이는 문자열을 다루는 데 있어서 매우 중요한 요소이며, Swift가 이를 자동으로 처리하므로 우리는 걱정할 필요가 없습니다.

문자열의 길이를 찾는 `count` 메서드는 문자열 내의 모든 문자를 셀 수 있도록 스위프트에서 제공하는 강력한 기능 중 하나입니다. 이를 통해 우리는 문자열의 길이를 신속하고 쉽게 알 수 있습니다.

## 관련 정보

- [Swift 공식 문서 (문자열 처리)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift 문자열 처리 예제](https://www.hackingwithswift.com/example-code/strings/how-to-find-the-length-of-a-string)
- [Swift 문자열 다루기 (영상)](https://www.youtube.com/watch?v=s-Vm30MrAaQ)