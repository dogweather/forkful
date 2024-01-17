---
title:                "텍스트 검색 및 대체"
html_title:           "Swift: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
검색 및 교체 텍스트는 프로그래머가 코드에서 특정 텍스트를 찾아 다른 텍스트로 대체하는 것을 말합니다. 이것은 버그 수정, 유지 보수 또는 효율적인 코드 작성을 위해 사용될 수 있습니다.

## 어떻게:
텍스트 검색 및 교체는 매우 간단합니다. 먼저 검색할 텍스트를 지정하고, 이를 바꿀 텍스트를 지정한 후에 코드를 실행해주면 됩니다. 아래는 Swift에서 검색 및 교체하는 간단한 예제 코드입니다.

```Swift
let originalString = "Hello, world!"
let replacedString = originalString.replacingOccurrences(of: "world", with: "Swift")
print(replacedString) // 출력 결과: "Hello, Swift!"
```

이 코드에서, 우리는 `originalString`이라는 문자열에서 "world"를 "Swift"로 대체했습니다. 이제 `replacedString`에는 "Hello, Swift!"라는 문자열이 저장되어 있습니다.

## 깊이 들어가보기:
텍스트 검색 및 교체는 많은 프로그래밍 언어에서 기본 제공되는 기능입니다. 하지만 Swift에서는 Apple이 제공하는 `replacingOccurrences(of:with:)` 함수를 사용하여 보다 쉽고 간단하게 텍스트를 검색하고 교체할 수 있습니다.

또한, 정규식(regular expression)과 같은 다른 방법을 사용하여 보다 복잡한 검색 및 교체를 수행할 수도 있습니다. 하지만 정규식은 복잡하고 이해하기 어렵기 때문에, 적절한 지식 없이는 사용하기 어렵습니다.

## 참고하기:
- Swift 공식 문서: [String.replacingOccurrences(of:with:)](https://developer.apple.com/documentation/foundation/nsstring/1407767-replacingoccurrences)
- 정규식(regular expression) 사용법: [정규식 완전 해부](https://regexper.com/)