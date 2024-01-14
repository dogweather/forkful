---
title:    "Swift: 디버그 출력 출력"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜?

디버그 출력을 활용하는 이유는 우리가 우리 코드의 작동을 이해하고 문제를 해결하기 위해서입니다. 디버깅은 코드의 오류를 찾고 수정하는 과정에서 매우 중요한 역할을 합니다.

## 방법

우선, `print()` 함수를 사용하여 디버그 출력을 할 수 있습니다. 이 함수를 사용하여 변수의 값을 확인하거나 코드를 실행하기 전에 알림 메시지를 출력할 수 있습니다. 예시 코드는 다음과 같이 작성할 수 있습니다:

```swift
let name = "John"
print("Hello, my name is \(name).")
```

위 코드를 실행하면 다음과 같은 출력 결과를 볼 수 있습니다:

```
Hello, my name is John.
```

또 다른 유용한 방법은 `DebugPrintable` 프로토콜을 준수하는 커스텀 타입을 만드는 것입니다. 이 프로토콜을 준수하는 타입은 `debugDescription` 프로퍼티를 가지고 있으며, 이를 통해 디버그 출력을 커스텀할 수 있습니다. 예시 코드는 다음과 같습니다:

```swift
struct Person: DebugPrintable {
    let name: String
    let age: Int
    
    var debugDescription: String {
        return "Name: \(name), Age: \(age)"
    }
}

let person = Person(name: "Jane", age: 25)
print(person)
```

위 코드를 실행하면 다음과 같은 출력 결과를 볼 수 있습니다:

```
Name: Jane, Age: 25
```

## Deep Dive

디버그 출력은 우리가 코드를 디버깅하는 과정에서 가장 유용한 도구 중 하나입니다. 하지만, 실제 프로덕션 환경에서는 디버그 출력을 많이 사용하지 않는 것이 좋습니다. 디버그 출력 코드는 오류를 찾고 수정하는 과정에서 사용하기 위한 것이므로, 디버그 출력 코드를 실제로 배포하는 것은 코드의 성능을 저하시킬 수 있습니다.

또한, 스위프트에서는 `assert()` 함수를 사용하여 디버그 출력을 좀 더 쉽게 관리할 수 있습니다. 이 함수는 조건을 체크하고 조건이 참이 아니면 디버그 출력을 할 수 있도록 해줍니다. 이를 통해 디버그 출력을 활성화 또는 비활성화할 수 있습니다.

## See Also

- [Apple Developer Documentation - Debugging with Xcode](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/debugging_tools.html)
- [Swift.org - Debugging](https://swift.org/blog/debugging/)