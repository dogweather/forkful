---
title:                "문자열의 길이 찾기"
date:                  2024-01-20T17:48:16.224857-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

문자열의 길이를 구한다는 것은, 문자열에 포함된 문자의 수를 셉니다. 코드가 사용자의 입력이나 데이터를 올바로 처리하고 있는지 확인하거나, 사용자 인터페이스에 문자열을 적절히 표시하도록 하기 위해서 문자열의 길이를 파악해야 할 때가 있죠.

## How to: (방법:)

Swift에서 문자열의 길이를 찾는 것은 간단합니다. 예제 코드를 통해 보세요.

```Swift
let greeting = "안녕하세요!"
let length = greeting.count
print("문자열의 길이: \(length)")
```

실행 결과:
```
문자열의 길이: 6
```

단순히 `count` 프로퍼티를 사용하여 문자열의 길이를 가져올 수 있습니다.

## Deep Dive (심층 분석)

과거 Objective-C와 같은 언어에서는 문자열의 길이를 구하는 것이 Swift보다 복잡했습니다. `length` 속성이나 `NSString` 클래스를 사용해야 했죠. Swift는 Unicode를 완벽히 지원하므로, `count`를 사용할 때 각 유니코드 문자를 정확히 세어 길이를 제공합니다.

길이를 찾는 또 다른 방법으로, 문자열을 배열로 변환 후 그 길이를 세는 방법도 있습니다. 하지만, `count` 프로퍼티가 더 직관적이고 효율적입니다. 확장 문자 집합 명세에 따라, 하나의 "문자"가 여러 유니코드 스칼라로 이루어져 있을 수 있는데, 이런 복잡한 상황에서도 Swift의 `count`는 올바른 값을 반환하죠.

문자열의 길이를 구할 때, 네트워킹 작업이나 파일 입출력, UI 처리에 있어서 매우 유용합니다. 긴 문자열이나 여러 줄의 텍스트를 다룰 때, 문자열의 길이를 알면 메모리 관리나 사용자 경험 개선에 있어서 중요하게 작용합니다.

## See Also (참고 자료)

- Swift 공식 문서의 문자열 및 문자 섹션: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- 유니코드 문자열 처리에 대한 정보: [Unicode](https://unicode.org)