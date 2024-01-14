---
title:    "Swift: 패턴이 일치하는 문자 삭제하기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜

Characters matching a pattern를 삭제하는 방법을 사용하는 이유는 무엇일까요? 일반적으로는 문자열 내에서 특정한 패턴이나 문자열을 제거하고 싶을 때 사용합니다. 예를 들어, 개인 정보를 포함한 문자열을 보안 상의 이유로 제거하려는 경우에 유용합니다.

## 어떻게

``` Swift
let inputString = "John's social security number is 123-45-6789."
let pattern = "[0-9-]*"
let modifiedString = inputString.replacingOccurrences(of: pattern, with: "", options: .regularExpression)
print(modifiedString)
// Output: John's social security number is .
```

위의 예제 코드를 보면, 입력된 문자열에서 숫자와 대쉬('-')로 이루어진 패턴을 찾아 제거하는 방법을 보여줍니다. `replacingOccurrences` 메서드를 사용해서 필요한 패턴을 찾고 제거할 수 있습니다.

## 깊이 파고들기

위에서 사용한 `replacingOccurrences` 메서드는 `options` 파라미터를 통해 정규표현식을 사용할 수 있습니다. 이를 통해 더 복잡한 패턴을 찾아 제거할 수도 있으며, 여러 개의 찾고자 하는 패턴을 한 번에 처리할 수도 있습니다.

## 관련 자료

- [Apple Developer Documentation: String.replacingOccurrences](https://developer.apple.com/documentation/foundation/string/1787541-replacingoccurrences)
- [Swift by Sundell: Deleting substrings matching a regular expression](https://www.swiftbysundell.com/posts/deleting-substrings-matching-a-regular-expression-in-swift)
- [NSHipster: NSRegularExpression](https://nshipster.com/nsregularexpression/)