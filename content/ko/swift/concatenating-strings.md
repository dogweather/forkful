---
title:                "Swift: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

"## 왜"

문자열을 연결하는 것에 대해 알아볼까요? 왜냐하면 프로그래밍에서 문자열은 매우 중요한 개념이기 때문입니다.

## "## 방법"

다음은 간단한 Swift 코드를 사용하여 문자열을 연결하는 방법을 보여드리겠습니다.

```Swift
let string1 = "Hello"
let string2 = "world"
let greeting = string1 + string2

print(greeting)
```

출력 결과: Hello world

이 코드에서는 "+" 연산자를 사용하여 두 개의 문자열을 결합하고 새로운 변수에 저장합니다. 그리고 이 새로운 변수를 출력하면 두 문자열이 연결된 모습을 볼 수 있습니다.

## "## 깊게 파헤치기"

문자열을 연결하는 더 깊은 방법을 알아보겠습니다. Swift에서는 문자열을 다루는 많은 유용한 메소드들이 있습니다. 그 중에서도 두 문자열을 연결하는 메소드인 `joined()`를 사용해보겠습니다.

```Swift
let strings = ["This", "is", "a", "sentence"]
let combinedString = strings.joined(separator: " ")
print(combinedString)
```

출력 결과: This is a sentence

이 코드에서는 `joined()` 메소드를 사용하여 배열에 있는 문자열들을 띄어쓰기를 기준으로 연결하고, 그 결과를 `combinedString` 변수에 저장하고 출력합니다.

## "See Also"

- Apple Developer Documentation: [Concatenating Strings](https://developer.apple.com/documentation/swift/string/3244336-joined)
- Codecademy: [Combining Strings](https://www.codecademy.com/courses/introduction-to-swift/lessons/strings-operators/exercises/concat)
- Ray Wenderlich: [Swift String Interpolation: Concatenate and Format Strings Easily](https://www.raywenderlich.com/5598-swift-string-interpolation-tutorial#toc-anchor-001)