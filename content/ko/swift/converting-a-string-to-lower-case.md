---
title:                "Swift: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜
문자열을 소문자로 변환하는 작업을 수행하는 이유는 무엇인가요?

문자열을 소문자로 변환하는 작업은 대소문자를 구분하지 않는 일부 문자열 조작에 유용합니다. 예를 들어, 사용자가 입력한 검색어를 받아들일 때, 대소문자에 관계없이 일치하는 결과를 찾을 수 있도록 하기 위해서입니다.

# 방법
```Swift
let originalString = "Hello Swift Programmers"
let lowerCaseString = originalString.lowercased()
print(lowerCaseString)
```
```Hello Swift Programmers```

```Swift
let sentence = "This iS AN examPle"
let lowercasedSentence = sentence.lowercased()
print(lowercasedSentence)
```
```this is an example```

# 깊이 파고들기
문자열을 소문자로 변환하는 메서드에 대해 더 알아보겠습니다. Swift에서는 ```lowercased()``` 메서드를 사용하여 문자열을 소문자로 변환할 수 있습니다. 이 메서드는 새로운 문자열을 반환하므로 원래 문자열이 수정되지 않습니다. 또한, 문자열에 대해 이 메서드를 여러 번 호출해도 결과는 같습니다. 예를 들어, "HeLlo"를 2번 호출하면 "hello"가 반환됩니다. 또한, 알파벳이 아닌 문자나 아스키 이상의 문자도 대소문자가 있는 경우, 해당 문자는 변환되지 않고 결과 문자열에 그대로 포함됩니다.

# 참고 자료
[Apple Developer Documentation - lowercased()](https://developer.apple.com/documentation/swift/string/3019262-lowercased)\
[iOS Tutorial: Basics for Beginners - How to Convert Strings to Lowercase](https://www.ioscreator.com/tutorials/convert-string-lowercase-swift)\
[Stack Overflow - converting string to lower case in swift 4](https://stackoverflow.com/questions/40697787/converting-string-to-lower-case-in-swift-4)