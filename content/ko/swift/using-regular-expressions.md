---
title:                "Swift: 정규 표현식 사용하기"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜?

정규표현식을 사용하는 이유는 복잡한 문자열 패턴을 쉽게 처리하기 위해서입니다. 정규표현식은 매우 강력한 도구이며 여러분의 코드를 간결하고 효율적으로 만듭니다.

## 어떻게 해야 할까요?

정규표현식을 사용하여 문자열에서 특정 패턴을 찾는 방법은 매우 간단합니다. 먼저, 다음과 같이 ```Swift let pattern = "apple"``` 패턴을 정의합니다.

```Swift
let pattern = "apple"
```

그런 다음, ```~.match(text: String)``` 메서드를 사용하여 특정 텍스트에서 패턴을 찾을 수 있습니다. 예를 들어, "I love eating apples!" 이라는 텍스트에서 패턴을 찾기 위해서는 다음과 같이 코드를 작성할 수 있습니다.

```Swift
let text = "I love eating apples!"
let results = pattern.match(text: text)
print(results) // Output: ["apple"]
```

위 코드를 실행하면 이 코드에서 "apple"이라는 패턴을 발견한 것을 볼 수 있습니다.

## 깊이 알아보기

정규표현식은 강력한 도구이지만 항상 사용하기는 쉽지 않을 수 있습니다. 특히, 복잡한 패턴을 다룰 때는 더욱 그렇습니다. 따라서 몇 가지 유용한 팁을 공유하고자 합니다.

첫째, 패턴은 여러 개의 특수 문자를 사용하여 정의할 수 있습니다. 예를 들어, ```"a.b"``` 패턴을 정의하면 "a"와 "b" 사이에 어떤 문자가 오더라도 패턴이 일치하도록 할 수 있습니다.

둘째, 정규표현식은 옵셔널한 문자를 표현할 수 있습니다. 예를 들어, ```"a*"[^"b"]``` 패턴은 "a"로 시작하고 다음에 아무 문자가 올 수 있지만 "b"는 제외하도록 정의할 수 있습니다.

셋째, 정규표현식에는 모든 문자를 일치시키는 메타 문자인 "."이 있습니다. 따라서 모든 문자와 일치시키고 싶을 때는 다음과 같이 간단한 패턴을 사용할 수 있습니다.

```Swift
let pattern = "."
```

## 같이 보기

- [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regular Expression 101](https://regex101.com)
- [Swift Regular Expressions Tutorial](https://www.raywenderlich.com/316-regular-expressions-tutorial-getting-started-with-nsregularexpression)