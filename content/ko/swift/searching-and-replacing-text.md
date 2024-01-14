---
title:                "Swift: 텍스트 검색과 대체"
simple_title:         "텍스트 검색과 대체"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

이 블로그 글에서는 텍스트를 검색하고 바꾸는 일에 대해 알아보겠습니다. 프로그래머들은 이 기능을 이용하여 효율적으로 코드를 수정할 수 있습니다.

## 하는 방법

바꾸고자 하는 텍스트를 포함하고 있는 문자열에서 다른 텍스트로 바꾸는 방법은 아주 간단합니다. 먼저, 간단한 예제를 살펴보겠습니다.

```Swift
var text = "Hello, World!"
text = text.replacingOccurrences(of: "World", with: "Swift")
print(text)
```

출력 결과는 다음과 같습니다:

```
Hello, Swift!
```

위의 코드는 `replacingOccurrences` 메소드를 이용하여 문자열에서 "World"를 "Swift"로 바꾸었습니다. 이 메소드는 기존 문자열을 수정하지 않고 새로운 문자열을 반환합니다.

또 다른 예제를 살펴보겠습니다. 이번에는 특정 패턴을 가진 문자열을 다른 문자열로 바꾸는 방법을 알아보겠습니다.

```Swift
let sentence = "I love to code in Swift!"
let modifiedSentence = sentence.replacingOccurrences(of: "love to code in", with: "enjoy programming in")
print(modifiedSentence)
```

출력 결과는 다음과 같습니다:

```
I enjoy programming in Swift!
```

위의 코드에서는 `replacingOccurrences` 메소드를 이용하여 "love to code in" 패턴을 "enjoy programming in"으로 바꾸었습니다.

## 깊이 들어가기

위의 예제들에서는 `replacingOccurrences` 메소드를 이용하여 간단하게 텍스트를 바꾸는 방법을 살펴보았습니다. 이제는 이 메소드의 깊은 내부를 살펴보겠습니다. `replacingOccurrences` 메소드는 String 클래스의 확장 프로퍼티로서 구현되어 있습니다.

```Swift
extension String {
    func replacingOccurrences(of target: String, with replacement: String) -> String {
        var temp = self
        while let foundRange = temp.range(of: target) {
            temp.replaceSubrange(foundRange, with: replacement)
        }
        return temp
    }
}
```

위의 코드에서는 `range(of:)` 메소드를 이용하여 주어진 문자열의 인덱스를 찾고, `replaceSubrange(_:with:)` 메소드를 이용하여 해당 인덱스에서 문자열을 대체합니다. 이러한 과정을 반복하여 원하는 결과를 반환합니다.

## 참고 자료

- [Apple 공식문서 - String.replacingOccurrences(of:with:)](https://developer.apple.com/documentation/swift/string/1786175-replacingoccurrences)
- [Swift Docs - Extending Types](https://docs.swift.org/swift-book/LanguageGuide/Extensions.html)
- [Apple 공식문서 - String](https://developer.apple.com/documentation/swift/string#relationships)
- [Swift | Strings](https://www.programiz.com/swift-programming/strings)