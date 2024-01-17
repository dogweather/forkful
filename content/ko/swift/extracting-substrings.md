---
title:                "부분 문자열 추출하기"
html_title:           "Swift: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 어떤 것 & 왜?

서브스트링 추출이란 무엇인가요? 이것은 문자열에서 일부분만 잘라내는 것을 말합니다. 프로그래머들은 이 작업을 수행해야하는 이유는 일련의 문자열 중에서 필요한 부분만 선택해 사용하기 위해서입니다.

## 어떻게:

```Swift
let str = "Hello World!"
let start = str.index(str.startIndex, offsetBy: 1)
let end = str.index(str.endIndex, offsetBy: -1)
let substring = str[start..<end]
print(substring)
// Output: ello World
```

위 예제에서는 문자열 "Hello World!"의 두 번째부터 마지막 바로 전까지의 서브스트링을 추출하는 방법을 보여줍니다. 이를 위해 우리는 문자열의 시작과 끝 인덱스를 찾고, offsetBy를 사용하여 원하는 위치를 지정합니다. 마지막으로, start..<end 문법을 사용하여 해당 서브스트링을 추출합니다.

## 깊게 들어가보기:

서브스트링 추출은 문자열 조작에서 흔히 사용되는 기술입니다. 예를 들어, 사용자의 이름을 받아서 성의 첫 글자만 보여주고 싶을 때, 이 기술을 활용할 수 있습니다. 또한 서브스트링은 문자열 검색과 비교에도 유용하게 사용됩니다. Swift에서는 문자열을 다루는 데 많은 메소드와 기능을 제공하기 때문에, 다양한 방법으로 서브스트링을 추출할 수 있습니다.

## 관련 자료:

- [Apple Swift Documentation on Substrings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Swift By Sundell: Substrings in Swift](https://www.swiftbysundell.com/basics/substrings-in-swift/)