---
title:                "Swift: 영어 문장의 첫 글자 대문자로 변경하기"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

String의 첫 글자를 대문자로 바꿀 필요가 있을 때가 있습니다. 예를 들어, 사용자 이름을 입력받아서 문자열을 출력할 때, 이름의 첫 글자를 대문자로 시작하는 것이 좀 더 예쁜 출력을 만들어줄 수 있습니다. 또는 데이터베이스에서 가져온 문자열이 모두 소문자인데, 그 중 일부를 대문자로 변환하여 일관성 있는 데이터를 유지할 수도 있습니다. 이러한 경우에는 String의 capitalize() 메서드를 사용할 수 있습니다.

## 방법

Swift에서는 String을 capitalize하는 두 가지 방법이 있습니다. 첫 번째는 capitalize() 메서드를 사용하는 것이며, 다음과 같은 형식을 따릅니다.

```Swift
let name = "jane"
let capitalized = name.capitalize() // "Jane"
print(capitalized) // Jane
```

또 다른 방법은 첫 번째 문자를 직접 대문자로 변환하는 것입니다. 이 방법은 다음과 같이 구현할 수 있습니다.

```Swift
var name = "jane"
name.replaceSubrange(name.startIndex...name.startIndex, with: String(name[name.startIndex]).capitalized)
print(name) // Jane
```

## 깊이 파고들기

하지만 문자열을 대문자로 바꾸는 것은 쉬운 일이 아닙니다. 예를 들어, 세 번째 문자를 대문자로 바꾸려고 할 때, 첫 번째 문자를 임시 변수에 저장하고 나서 문자열을 잘라서 다시 합치는 등 복잡한 과정을 거쳐야 합니다. 따라서, Swift에서는 이런 경우를 대비하여 Capitalizing라는 기능을 제공합니다. 사용 방법은 아래와 같습니다.

```Swift
let name = "john"
let capitalized = name.capitalizingPrefix(2) // "JOhn"
print(capitalized) // JOhn
```

이 외에도 Capitalizing은 여러 가지 옵션을 제공합니다. 자세한 내용은 [공식 문서](https://developer.apple.com/documentation/foundation/nsstring/3177247-capitalize)를 참고해주세요.

## 관련 링크

- [String Capitalization in Swift](https://www.hackingwithswift.com/swift4/string-capitalization) (영문)
- [Swift String Cheat Sheet](https://learnappmaking.com/swift-string-how-to/) (영문)
- [Swift 기본 문법: 문자열(String)](https://soooprmx.com/archives/4743) (한글)