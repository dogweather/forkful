---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 소문자로 변환한다는 것은 대문자를 포함한 문자열을 모두 소문자로 바꾸는 작업을 의미합니다. 이는 주로 사용자 입력의 일관성을 유지하거나, 검색 및 비교 작업을 더 쉽게 하기 위해 프로그래머들이 사용합니다.

## 어떻게?

Swift에서 문자열을 소문자로 변환하는 코드는 아래와 같습니다.

```Swift
let upperCaseString = "HELLO, WORLD!"
let lowerCaseString = upperCaseString.lowercased()
print(lowerCaseString)
```

이 코드를 실행하면, 출력창에는 "hello, world!"가 나타나게 됩니다.

## 깊이 들어가보기

기술적인 세부사항에서 보면, Swift 의 `lowercased()` 메소드는 Unicode 정의에 따라 문자열을 소문자로 변환합니다. 이는 여러 언어와 문화권을 대상으로 일관된 소문자 변환이 되도록 보장합니다.

대신에 `lowercased()` 메소드를 사용하지 않고 직접 소문자 변환을 구현하는 것은 권장하지 않습니다. 그 이유는 Unicode 문자의 다양성 때문에 다양한 국가 및 언어에서 확실한 소문자로의 변환을 보장하기 어렵기 때문입니다.

대안적으로 사용 가능한 메소드로 `lowercased(with: Locale?)`가 있습니다. 이 메소드는 지정된 로캘에 적합하게 문자열을 소문자로 변환해줍니다. 

## 참고 자료

담대한 Swift 알고리즘 클럽의 [String 확장](https://github.com/raywenderlich/swift-algorithm-club/tree/master/String%20Extensions)을 확인해 보세요. Swift의 [공식 문자열 및 문자 가이드](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)도 상세한 정보를 제공합니다.