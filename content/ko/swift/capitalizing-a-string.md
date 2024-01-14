---
title:    "Swift: 문자열 대문자 변환하기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 변환하는 것의 의미는 무엇일까요? 여러분이 일상 생활에서 문자열을 다루거나 프로그래밍을 할 때 이 작업을 수행하는 이유는 무엇일까요? 그것은 바로 사용자가 입력한 문자열을 일관적인 형식으로 표시하여 보기 쉽게 만들어주기 위해서입니다. 예를 들어, 사용자가 입력한 이름이나 제목을 대문자로 변환해주면 주목할 만한 효과를 만들 수 있습니다. 이제 이제 대문자를 어떻게 만들 수 있는지 살펴보겠습니다!

## 어떻게

Swift에서 문자열을 대문자로 변환하는 것은 매우 간단한 작업입니다. 먼저, 변환하려는 문자열을 String 형식으로 선언해야 합니다. 그리고 이제 `uppercased()` 메서드를 활용하여 대문자로 변환할 수 있습니다.

```
// 예제 코드
var str = "hello, world!"
str = str.uppercased()
print(str)

// 결과
HELLO, WORLD!
```

위의 예제 코드에서 보시는 바와 같이, `uppercased()` 메서드를 호출한 뒤 변환된 문자열을 다시 원래의 변수에 할당해야 합니다. 그렇지 않으면 변환된 문자열이 저장되지 않습니다.

## 깊게 들어가보기

문자열을 대문자로 변환하는 방법은 더 있는 것을 알고 계신가요? Swift에서는 `lowercased()` 메서드를 활용해 소문자로 변환할 수도 있습니다. 또한, 문자열을 배열로 변환한 뒤 각 문자를 개별적으로 대문자로 변환할 수도 있습니다. 이는 `map()` 메서드와 `uppercased()` 메서드를 함께 사용하여 구현할 수 있습니다.

```
// 예제 코드
let message = "hello, world!"
let stringArray = message.map { String($0).uppercased() }
let result = stringArray.joined(separator: "")

print(result)

// 결과
HELLO, WORLD!
```

이렇게 문자열을 배열로 변환하고 각 문자를 대문자로 변환한 뒤, 다시 합쳐주는 과정을 통해 모든 문자가 대문자로 변환된 문자열을 만들 수 있습니다. 이렇게 깊게 들어가는 것이 바로 프로그래밍의 매력이죠!

## 같이 보기

위의 예제 코드 외에도 Swift에서 문자열을 다루는 다양한 방법들이 있습니다. 아래의 링크들을 참고하여 더 많은 정보를 알아보세요!

[Swift 공식 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

[Tutorials Point](https://www.tutorialspoint.com/swift/swift_strings.htm)

[Codecademy](https://www.codecademy.com/learn/learn-swift/modules/learn-swift-strings)

많은 학습을 하신 후에는 여러분만의 창의적인 방법으로 문자열을 다루는 것도 좋은 아이디어입니다. 즐겁게 프로그래밍하시고, 더 많이 배워보세요!