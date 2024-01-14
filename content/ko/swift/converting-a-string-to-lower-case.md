---
title:    "Swift: 잉분 용자 졍샛 낮쫒"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는것에 대해 1-2 문장만으로 왜 이 작업을 해야하는지 설명합니다.

문자열을 소문자로 변환하는것은 자주 사용되는 프로그래밍 작업입니다. 대소문자를 무시하는 검색이나 비교를 할 때 유용하며, 특정 언어나 규칙에 따라 대문자를 사용해야 할 때 유용합니다. 또한, 사용자 입력을 정제하거나 출력 결과를 일관성있게 유지하는 데에도 도움이 됩니다.

## 어떻게

`lowercased()` 메서드를 사용하여 Swift에서 문자열을 소문자로 변환하는 방법은 다음과 같이 간단합니다.

```Swift
let greeting = "Hello World!"
let lowercaseGreeting = greeting.lowercased()
print(lowercaseGreeting)
// 출력 결과: hello world!
```

위의 예제는 `lowercased()` 메서드를 변수 `greeting`에 사용하여 `lowercaseGreeting` 변수에 저장하고, 이를 `print` 함수를 사용하여 콘솔에 출력하는 간단한 코드입니다.

## 깊이 파고들기

Swift의 `lowercased()` 메서드는 내부에서 내장 C 함수인 `tolower()`를 호출하여 소문자로 변환합니다. 이 함수는 UTF-16 인코딩을 사용하여 문자열의 모든 문자를 소문자로 변환합니다. 따라서, 문자가 아닌 기호나 숫자는 유지되며 모든 하위 문자가 소문자로 변환됩니다.

따라서, `lowercased()` 메서드를 호출할 때 주의할 점은 원래의 문자열이 어떤 인코딩으로 되어있는지 확인하는 것입니다. 만약 UTF-8 인코딩을 사용하고 있다면 이 메서드는 맞는 결과를 반환하지 않을 수 있습니다. 이럴 경우에는 `precomposedStringWithCanonicalMapping` 메서드를 사용하여 정규화된 문자열을 생성한 뒤, 이를 `lowercased()` 메서드를 사용하여 소문자로 변환할 수 있습니다.

## 관련 자료

- [Apple Developer Documentation - Character](https://developer.apple.com/documentation/swift/character)
- [Apple Developer Documentation - String](https://developer.apple.com/documentation/swift/string)
- [String metamorphosis - Lowercase letters](https://medium.com/bochs-projects/string-metamorphosis-26734f03a2e3)