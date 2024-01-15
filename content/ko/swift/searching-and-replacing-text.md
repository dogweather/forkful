---
title:                "텍스트 검색 및 대체"
html_title:           "Swift: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 찾아 바꾸는 것의 이유는 프로그램에서 자주 사용하는 동작이며 효율적인 코딩을 위해 필수적입니다.

## 어떻게

텍스트를 찾아 바꾸기 위해서는 `replacingOccurrences(of:with:)` 메서드를 사용해야 합니다. 이 메서드는 원하는 문자를 지정한 문자로 모두 바꿔주는 기능을 합니다. 아래는 간단한 예시 코드입니다.

```Swift
let sentence = "Hello Swift!"
let newSentence = sentence.replacingOccurrences(of: "Swift", with: "Kotlin")
print(newSentence) // Output: Hello Kotlin!
```

문자열 내에서 모든 "Swift"를 "Kotlin"으로 바꾸어줍니다. 또 다른 예시로는 `replacingOccurrences(of:with:options:range:)` 메서드를 사용하는 방법이 있습니다. 이 메서드는 옵션을 추가하여 대소문자를 구분하거나 특정 범위에서만 문자를 바꿀 수 있습니다. 아래는 이 방법을 사용한 예시 코드입니다.

```Swift
let sentence = "Hello swift!"
let newSentence = sentence.replacingOccurrences(of: "swift", with: "Kotlin", options: .caseInsensitive, range: nil)
print(newSentence) // Output: Hello Kotlin!
```

여기서 옵션에 `.caseInsensitive`를 추가하였기 때문에 대소문자를 구분하지 않고 "swift"를 "Kotlin"으로 바꿔줍니다. 또한 마지막 매개변수로 범위를 지정해줄 수도 있습니다.

## 딥 다이브

텍스트를 찾아 바꾸는 과정에서 문제가 발생할 수 있습니다. 예를 들어, 바꾸려는 문자열을 찾지 못해 원하는 결과를 얻지 못할 수 있습니다. 이때는 `replacingOccurrences(of:with:options:range:)` 메서드의 리턴 값으로 사용되는 `String`을 가져와서 `contains()` 메서드를 사용하여 원하는 문자열이 포함되어 있는지 확인할 수 있습니다. 아래는 이 방법을 사용한 예시 코드입니다.

```Swift
let sentence = "Hello swift!"
let newSentence = sentence.replacingOccurrences(of: "Swift", with: "Kotlin", options: .caseInsensitive, range: nil)

if newSentence.contains("Kotlin") {
    print("Found 'Kotlin'!") // Output: Found 'Kotlin'!
} else {
    print("Could not find 'Kotlin'!")
}
```

`contains()` 메서드를 사용하여 "Kotlin"이 포함되어 있는지를 확인하고 그에 따라 적절한 메시지를 출력합니다.

## 참고

- [Apple Developer Documentation: String](https://developer.apple.com/documentation/swift/string)
- [Hacking With Swift: How to use replacingOccurrences() to replace text in a string](https://www.hackingwithswift.com/example-code/strings/how-to-use-replacingoccurrences-to-replace-text-in-a-string) 
- [Swift by Sundell: Replacing text using ranges in Swift](https://www.swiftbysundell.com/posts/replacing-text-using-ranges-in-swift)