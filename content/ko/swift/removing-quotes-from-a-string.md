---
title:                "문자열에서 따옴표 제거하기"
date:                  2024-01-26T03:42:35.711953-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 따옴표 제거하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 따옴표를 제거한다는 것은 내용을 감싸고 있는 모든 인용 부호를 없애는 것을 의미합니다. 이 작업을 수행하는 이유는 입력값을 정제하거나, 데이터를 저장할 준비를 하거나, 데이터 처리에 방해가 될 수 있는 불필요한 텍스트 포맷팅을 없애기 위해서입니다.

## 방법:

Swift는 따옴표 제거 작업을 꽤 손쉽게 처리할 수 있게 해줍니다. 다음은 `replacingOccurrences(of:with:)`를 사용한 간단한 예제로, 이름에서 알 수 있듯이 텍스트의 일부를 다른 것으로 교체하거나 아예 없애는 작업을 수행합니다.

```swift
var quotedString = "\"This is a 'quoted' string.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // This is a 'quoted' string.

// 단일 따옴표를 다루고 싶다면, 검색어를 변경하기만 하면 됩니다.
quotedString = "'Here's another example.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Heres another example.
```

출력 결과는 어떤 계획을 가지고 있든 준비가 된 따옴표 없는 문자열이 될 것입니다.

## 심층 탐구

프로그래밍이 시작된 이래로 우리는 이러한 문자열을 "정리"하는 작업을 해왔습니다. 초기에는 귀중한 메모리를 절약하고 입력 처리 시 구문 오류를 피하는 것이 더 중요했습니다. 오늘날에는 특히 JSON을 다루거나 데이터베이스 작업을 위한 문자열을 준비할 때 좋은 데이터 위생에 관한 문제입니다. 잘못된 따옴표 하나가 SQL 쿼리를 빠르게 망칠 수 있습니다. "구문 오류"라고 말하기도 전에요.

대안은? 만약 `replacingOccurrences(of:with:)`가 너무 평범하다고 생각된다면, 더 복잡한 패턴을 위해 정규 표현식을 탐구하거나, 특정 위치에서만 따옴표를 제거하고자 할 때 정규 표현식을 사용할 수 있습니다. 여기서 Swift의 `NSRegularExpression` 클래스가 도움이 됩니다. 하지만 정규 표현식은 강력하지만 때로는 과잉일 수 있는 양날의 검입니다.

구현 측면에서, `replacingOccurrences(of:with:)`는 Swift에서 `String`에 의해 제공되는 메소드로, 내부적으로는 유니코드와 현대 텍스트 처리의 다른 복잡성을 다루는 더 복잡한 문자열 조작 함수들을 호출합니다. 이것은 Swift가 당신을 대신해서 다루는 "표면상으로는 단순하지만 내부적으로는 복잡한" 문제 중 하나입니다.

## 참고 자료

Swift에서의 문자열 조작에 대한 더 많은 정보를 위해서:

- Swift 프로그래밍 언어 (문자열 및 문자): [Swift.org 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [Apple 개발자 문서](https://developer.apple.com/documentation/foundation/nsregularexpression)

그리고 정규 표현식에 대해 궁금하고 패턴을 테스트하고 싶다면:

- Regex101: [정규 표현식 테스터 및 디버거](https://regex101.com)