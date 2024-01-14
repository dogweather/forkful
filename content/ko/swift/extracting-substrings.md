---
title:    "Swift: 부분 문자열 추출"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# 왜

문자열에서 부분 문자열을 추출하는 방법에 대해 배우는 것이 왜 중요한지 궁금하셨나요? 이 기술은 유용한 데이터를 추출하고 예측 모델을 구축하는 데 필수적입니다. 또한 문자열의 특정 부분에 접근하는 데 도움이 됩니다.

## 어떻게

부분 문자열을 추출하는 것은 매우 간단합니다. 먼저 `String` 타입의 변수를 선언하고 문자열을 할당합니다. 그런 다음 `substring` 메서드를 사용하여 원하는 부분 문자열을 지정합니다.

## 코드 예제

```swift
let str = "Hello, World! 나는 Swift를 공부하고 있습니다."
let substring = str.substring(from: 7)
print(substring)

// Output: World! 나는 Swift를 공부하고 있습니다.
```

문자열에서 원하는 범위를 지정하면 더욱 간단합니다.

```swift
let str = "abcdefg"
let substring = str.substring(with: 3..<6)
print(substring)

// Output: def
```

## 더 깊게

부분 문자열을 추출하는 기술은 `String` 타입의 많은 유용한 메서드 중 하나입니다. 다음의 추가 메서드를 사용할 수 있습니다.

- `prefix(length)` : 문자열의 앞부분에서 지정된 길이만큼의 부분 문자열을 반환합니다.
- `suffix(length)` : 문자열의 뒷부분에서 지정된 길이만큼의 부분 문자열을 반환합니다.
- `capitalized` : 문자열의 첫 글자를 대문자로 바꾼 부분 문자열을 반환합니다.

# 관련 자료

- [Apple Developer Documentation | String Operations](https://developer.apple.com/documentation/swift/string)
- [Hacking with Swift | How to extract substrings](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string-using-indexing)
- [raywenderlich | String Cheat Sheet](https://www.raywenderlich.com/141888/string-cheat-sheet-swift-3-edition)