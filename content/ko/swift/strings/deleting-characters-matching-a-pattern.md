---
date: 2024-01-20 17:43:25.769388-07:00
description: "How to: (\uBC29\uBC95) Swift\uC5D0\uC11C \uC815\uADDC\uC2DD\uC744 \uC0AC\
  \uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC5D0\uC11C \uD328\uD134\uC5D0 \uB9DE\uB294\
  \ \uBB38\uC790\uB97C \uC0AD\uC81C\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC608\uB97C\
  \ \uB4E4\uC5B4, \uC22B\uC790\uB9CC \uCD94\uCD9C\uD558\uACE0\uC790 \uD560 \uB54C\
  \ \uB2E4\uC74C\uACFC \uAC19\uC774 \uD560 \uC218 \uC788\uC8E0."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.329401-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Swift\uC5D0\uC11C \uC815\uADDC\uC2DD\uC744 \uC0AC\uC6A9\uD558\
  \uC5EC \uBB38\uC790\uC5F4\uC5D0\uC11C \uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\
  \uB97C \uC0AD\uC81C\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

## How to: (방법)
Swift에서 정규식을 사용하여 문자열에서 패턴에 맞는 문자를 삭제할 수 있습니다. 예를 들어, 숫자만 추출하고자 할 때 다음과 같이 할 수 있죠:

```Swift
import Foundation

func deleteMatchingCharacters(from string: String, pattern: String) -> String {
    let regex = try! NSRegularExpression(pattern: pattern)
    let range = NSRange(string.startIndex..., in: string)
    return regex.stringByReplacingMatches(in: string, options: [], range: range, withTemplate: "")
}

// 숫자가 아닌 모든 문자를 삭제하는 예시
let originalString = "Swift 5.2 버전에서 새로운 기능이 추가되었습니다!"
let pattern = "[^0-9]"
let resultString = deleteMatchingCharacters(from: originalString, pattern: pattern)

print(resultString) // 출력: 52
```

위 코드는 숫자를 제외한 모든 문자를 제거하여 결과를 반환합니다.

## Deep Dive (심층 분석)
과거에는 Swift에서 정규식을 사용하기 위해 `NSRegularExpression` 클래스를 Objective-C 브릿지를 통해 사용했습니다. 이제는 강력한 문자열 처리 기능을 가진 `String`과 `RegularExpression` 제공 받으며, Swift 네이티브 API를 통해 정규식을 쓸 수 있습니다.

`NSRegularExpression`을 사용할 때는 패턴을 만들고, 범위를 정하며, 문자열을 변환하는 작업이 복잡할 수 있어요. 하지만 효율적인 데이터 처리를 위해 이 방법을 자주 사용합니다.

패턴 매칭의 삭제 외에도, Swift에는 문자열을 다루기 위한 다양한 방법이 있습니다. `String`의 `replacingOccurrences(of:with:)` 함수로 간단한 문자 교체가 가능하고, `filter` 함수를 사용하여 문자열을 필터링할 수도 있습니다.

## See Also (참조)
- Swift 공식 문서 내 `String` 처리 부분: [Apple's String Documentation](https://developer.apple.com/documentation/swift/string)
- 정규식에 대한 개념과 패턴 작성 방법: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)

Swift를 사용한 문자열 처리와 데이터 정제는 매우 강력합니다. 제대로 이해하고 사용한다면 프로그램의 효율성을 크게 향상시킬 수 있습니다.
