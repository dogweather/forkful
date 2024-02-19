---
aliases:
- /ko/swift/finding-the-length-of-a-string/
date: 2024-01-20 17:48:16.224857-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uAD6C\uD55C\uB2E4\uB294\
  \ \uAC83\uC740, \uBB38\uC790\uC5F4\uC5D0 \uD3EC\uD568\uB41C \uBB38\uC790\uC758 \uC218\
  \uB97C \uC149\uB2C8\uB2E4. \uCF54\uB4DC\uAC00 \uC0AC\uC6A9\uC790\uC758 \uC785\uB825\
  \uC774\uB098 \uB370\uC774\uD130\uB97C \uC62C\uBC14\uB85C \uCC98\uB9AC\uD558\uACE0\
  \ \uC788\uB294\uC9C0 \uD655\uC778\uD558\uAC70\uB098, \uC0AC\uC6A9\uC790 \uC778\uD130\
  \uD398\uC774\uC2A4\uC5D0 \uBB38\uC790\uC5F4\uC744 \uC801\uC808\uD788 \uD45C\uC2DC\
  \uD558\uB3C4\uB85D \uD558\uAE30 \uC704\uD574\uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\
  \uC774\uB97C \uD30C\uC545\uD574\uC57C \uD560 \uB54C\uAC00 \uC788\uC8E0."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.735231
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uAD6C\uD55C\uB2E4\uB294 \uAC83\
  \uC740, \uBB38\uC790\uC5F4\uC5D0 \uD3EC\uD568\uB41C \uBB38\uC790\uC758 \uC218\uB97C\
  \ \uC149\uB2C8\uB2E4. \uCF54\uB4DC\uAC00 \uC0AC\uC6A9\uC790\uC758 \uC785\uB825\uC774\
  \uB098 \uB370\uC774\uD130\uB97C \uC62C\uBC14\uB85C \uCC98\uB9AC\uD558\uACE0 \uC788\
  \uB294\uC9C0 \uD655\uC778\uD558\uAC70\uB098, \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\
  \uC774\uC2A4\uC5D0 \uBB38\uC790\uC5F4\uC744 \uC801\uC808\uD788 \uD45C\uC2DC\uD558\
  \uB3C4\uB85D \uD558\uAE30 \uC704\uD574\uC11C \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\
  \uB97C \uD30C\uC545\uD574\uC57C \uD560 \uB54C\uAC00 \uC788\uC8E0."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
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
