---
title:    "Swift: 패턴과 일치하는 문자 삭제"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜?

문자열 내에서 패턴과 일치하는 문자를 삭제하는 것의 목적은 다양합니다. 예를 들어, 데이터 정제나 원하는 결과물을 얻기 위해서가 있습니다.

## 어떻게?

패턴 매칭 문자 삭제를 위해, Swift의 내장 함수 중에서 `replacingOccurrences(of:with:options:)`를 사용하면 편리합니다. 다음은 해당 함수를 사용하는 예제 코드와 결과물입니다:

```Swift
let str = "안녕하세요, Swift 프로그래밍에 오신 것을 환영합니다."
let pattern = "Swift"
let newStr = str.replacingOccurrences(of: pattern, with: "")
print(newStr)
// 출력: 안녕하세요, 프로그래밍에 오신 것을 환영합니다.
```

이 예제에서는 문자열 `str`에서 `Swift`라는 패턴과 일치하는 문자를 모두 삭제하고, 남은 문자열을 `newStr`에 대입한 후 출력하였습니다.

또한, 정규식을 사용하여 조금 더 복잡한 패턴 매칭을 할 수도 있습니다. 예를 들어, 숫자와 문자가 섞인 패턴일 경우 정규식을 사용해 쉽게 패턴을 찾고 삭제할 수 있습니다.

```Swift
let str = "1a2b3c 4d5e6f"
let pattern = "[0-9]"
let newStr = str.replacingOccurrences(of: pattern, with: "", options: .regularExpression, range: nil)
print(newStr)
// 출력: abc def
```

이 예제에서는 숫자를 찾는 정규식 패턴 `[0-9]`를 사용하여 문자열 `str`에서 모든 숫자를 찾아 삭제하고, 남은 문자열을 `newStr`에 대입한 후 출력하였습니다. 이외에도 `options`을 사용하여 대소문자 구분을 무시하거나 특수 문자를 삭제하는 등 다양한 옵션을 설정할 수 있습니다. 

## Deep Dive

`replacingOccurrences(of:with:options:)` 함수의 `range` 파라미터를 이용하면 문자열 내에서 일부분만 패턴 매칭을 할 수 있습니다. 이를 통해 특정 부분만 삭제할 수 있습니다. 또한, 해당 함수 뿐만 아니라 Swift에서 제공하는 다른 문자열 관련 함수들을 함께 사용하면 더욱 다양한 패턴 매칭 및 문자열 처리가 가능합니다.

## See Also

- [Swift 공식 문서 - 문자열 찾기 및 바꾸기](https://developer.apple.com/documentation/foundation/nsstring/1413289-replacingoccurrences)
- [Swift 공식 문서 - 정규식](https://developer.apple.com/documentation/foundation/nsregularexpression)