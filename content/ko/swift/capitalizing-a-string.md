---
title:                "문자열 대문자로 변환하기"
html_title:           "Swift: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

대부분의 프로그래밍 언어에서 문자열의 대소문자를 변환하는 기능은 자주 사용됩니다. Swift에서는 이를 쉽게 수행할 수 있도록 제공하는 `capitalized` 메서드가 있습니다. 이 기능을 사용하는 이유는 소문자로 입력된 문자열을 첫 글자를 대문자로 변경해주어 보기 좋거나, 대문자로 입력된 문자열을 소문자로 변경하여 일관된 형식을 유지하기 위함입니다.

## 방법

우선 `capitalized` 메서드를 실제로 사용해보기 위해 `str`이라는 변수에 문자열을 할당해줍니다. 이후 `str` 변수 뒤에 `.capitalized`를 붙여주면 해당 문자열이 첫 글자를 대문자로 변경된 상태로 반환됩니다. 예시 코드는 아래와 같습니다.

```Swift
let str = "swift programming"
print(str.capitalized)

// Output: Swift Programming
```

보다시피 `capitalized` 메서드를 사용하면 손쉽게 문자열의 대소문자를 변경할 수 있습니다. 또한 이 메서드는 첫 글자가 아닌 나머지 문자열의 소문자는 그대로 유지됩니다.

## 깊이 파헤치기

실제로 `capitalized` 메서드의 내부는 어떻게 구현되어 있을까요? 이 메서드는 문자열의 첫 번째 단어를 대문자로 변경한 후 나머지 문자열을 소문자로 변경하는 과정을 거쳐 동작합니다. 따라서 첫 글자가 아닌 나머지 문자열의 대소문자는 그대로 유지됩니다. 또한 이 메서드는 다중 언어를 지원하기 위해 유니코드를 사용하여 문자열을 분석하므로 다양한 언어에서도 정확하게 동작합니다.

## 관련 링크

- [Swift 공식 문서 - 문자열](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift 문자열 다루기 - 블로그 포스트](https://luxiana-medium.tistory.com/25)