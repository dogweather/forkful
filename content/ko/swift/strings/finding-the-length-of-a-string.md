---
date: 2024-01-20 17:48:16.224857-07:00
description: "How to: (\uBC29\uBC95:) Swift\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\
  \uC774\uB97C \uCC3E\uB294 \uAC83\uC740 \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uC608\uC81C\
  \ \uCF54\uB4DC\uB97C \uD1B5\uD574 \uBCF4\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.337268-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95:) Swift\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C\
  \ \uCC3E\uB294 \uAC83\uC740 \uAC04\uB2E8\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

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
