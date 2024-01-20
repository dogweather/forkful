---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
서브스트링 추출은 문자열 안의 특정 부분 문자열을 가져오는 것입니다. 이것은 프로그래머들이 텍스트 데이터를 관리하고 다루는 데 매우 중요합니다.

## 어떻게:
다음은 Swift에서 서브스트링을 추출하는 방법의 기본 예제입니다:

```Swift
let str = "Hello, Swift"
let start = str.index(str.startIndex, offsetBy: 7)
let end = str.index(str.startIndex, offsetBy: 12)
let substring = str[start..<end]
print(substring)  // Output: Swift
```

이 예제에서 `index(_:offsetBy:)` 함수는 주어진 문자열에서 특정 위치를 찾는데 사용됩니다. 이 함수를 사용하여 시작점과 끝점을 설정하고 이 인덱스를 사용하여 서브스트링을 추출합니다.

## 깊게 알아보기:
Swift가 서브스트링을 추출하는 방식의 히스토리는 Swift의 배경에서 찾을 수 있습니다. Swift 4 이전에는 문자열 인덱싱에 복잡성이 있었습니다. Swift 4에서 문자열 API가 개선되었고, 서브스트링 추출 기능이 더 간결하고 직관적으로 바뀌었습니다.

대안으로, `prefix(_:)` 및 `suffix(_:)` 메서드를 사용하여 문자열의 처음 또는 마지막 부분을 쉽게 가져올 수도 있습니다.  

```Swift
let str = "Hello, Swift"
let prefix = str.prefix(5)
print(prefix)  // Output: Hello

let suffix = str.suffix(5)
print(suffix)  // Output: Swift
```

Swift에서 서브스트링을 추출하는 것은 내부적으로 매우 효율적입니다. 이는 원본 문자열에 대한 참조를 보존하고 원본 문자열이 변경되지 않는 한 새로운 문자열을 만들지 않기 때문입니다. 이런 방식으로 메모리 사용량을 적게 유지할 수 있습니다.

## 더 보기:
다음의 추가 자료들이 Swift 문자열과 서브스트링에 대한 더 깊은 이해에 도움이 될 수 있습니다:

- [Apple의 공식 Swift 문자열 및 문자 가이드](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift의 문자열과 문자](https://www.hackingwithswift.com/quick-start/understanding-swift/what-is-a-string)라는 글에서는 문자열과 문자의 기본적인 사용법을 보여줍니다.
- [서브스트링 추출에 대한 Stack Overflow 토론](https://stackoverflow.com/questions/24192823/how-can-i-get-substring-from-string-with-swift)은 실제 개발자들이 이 문제를 어떻게 해결하고 있는지 보여줍니다.