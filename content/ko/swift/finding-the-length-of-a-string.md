---
title:                "Swift: 문자열의 길이 찾기"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜 필요할까요?

문자열의 길이를 찾는 것은 많은 Swift 프로그래머들이 길이를 제한하는 문자열 처리를 위해 필요합니다. 예를 들어, 사용자의 이름을 입력 받을 때, 입력된 이름이 지정된 길이보다 길면 에러 메시지가 나타나도록 설정할 수 있습니다. 문자열의 길이를 찾는 것은 유용한 기능이며, 우리는 이를 위해 어떻게 코드를 작성할 수 있는지 살펴보겠습니다.

## 어떻게 코드를 작성할 수 있을까요?

우선, 우리는 Swift의 `count` 메소드를 사용해서 문자열의 길이를 찾을 수 있습니다. 이 메소드는 문자열에 포함되어 있는 문자의 갯수를 반환하며, 예를 들어 `let str = "Hello, world!"` 라는 문자열이 있다고 가정해 봅시다. 해당 문자열의 길이를 찾기 위해서는 다음과 같이 작성할 수 있습니다.

```Swift
let str = "Hello, world!"
print(str.count)
```

위의 코드를 실행하면 `13` 이라는 결과를 얻게 됩니다. 이제 여러분은 문자열의 길이를 찾는 것이 매우 쉽다는 것을 알 수 있을 것입니다. 하지만 `count` 메소드는 문자열 내에 있는 문자의 상대적 위치를 포함하지는 않으므로 주의해야 합니다. 예를 들어, `let crazyStr = "🤪🎉!"` 라는 문자열이 있다고 가정해 봅시다. 해당 문자열의 길이는 5이지만, 실제로 `4`개의 문자를 포함하고 있습니다. 따라서 `count` 메소드는 4를 반환하게 됩니다.

위의 예시를 보면 이해하기 어려울 수 있지만, 간단한 예제를 통해 더 명확히 알아보겠습니다. 다음은 사용자로부터 이름을 입력받아 길이를 확인하는 간단한 코드입니다.

```Swift
print("이름을 입력해주세요:")
let name = readLine()
print("\(name!)의 길이는 \(name!.count) 입니다.")
```

위의 예제에서 `readLine()` 메소드를 통해 사용자로부터 입력을 받고, 해당 입력에 `count` 메소드를 적용하여 길이를 알 수 있습니다. 이제 문자열의 길이를 찾는 기초적인 방법을 알게 되었습니다.

## 깊이있게 알아보기

실제로 문자열의 길이를 찾는 구현 방식은 코드 내부에서 어떻게 동작할까요? 내부적으로 `count` 메소드는 문자열의 `unicodeScalars`를 검사하여 길이를 찾게 됩니다. `unicodeScalars`는 Swift에서 사용되는 모든 문자들의 `unicode` 값을 포함하는 배열입니다. 각 문자는 `10진수` 값으로 치환되어 있으며, 문자의 종류에 따라 다양한 값이 할당됩니다.

위에서 언급한 예제의 `let crazyStr = "🤪🎉!"` 라는 문자열은 `unicodeScalars` 배열로 변환하면 `[129310, 127881, 33]` 라는 값을 가지게 됩니다. 이 배열의 길이는 5이며, 실제 문자열의 길이와는 다릅니다. 따라서 `count` 메소드는 실제 문자열 길이가 아닌, 배열의 길이를 반환하게 됩니다.

## 다른 참고 자료

- [Swift 공식 문서 - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)