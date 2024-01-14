---
title:                "Swift: 패턴과 일치하는 문자 삭제하기"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

### 왜
문자열에서 특정 패턴과 일치하는 문자를 삭제하는 작업이 왜 필요한지 궁금하지 않으신가요? 이 글에서는 그 이유와 함께 해당 작업을 어떻게 수행하는지 알려드리겠습니다.

### 방법
자, 이제 문자열에서 패턴과 일치하는 문자를 삭제하는 방법에 대해 알아보겠습니다. 아래의 코드 블록을 참고하여 실제로 코드를 작성해보는 것이 이해에 도움이 될 것입니다.

```Swift
let text = "Hello World!"
let pattern = "[aeiou]"
let modifiedText = text.replacingOccurrences(of: pattern, with: "", options: .regularExpression, range: nil)
print(modifiedText) // Hll Wrld!
```

위의 코드는 "안녕하세요"라는 문자열에서 모음을 삭제하는 예시입니다. 우선 원본 문자열을 `text` 상수에 저장하고, 삭제할 패턴을 `pattern` 상수에 지정합니다. 그리고 `replacingOccurrences(of:with:options:range:)` 메서드를 사용하여 패턴과 일치하는 문자를 빈 문자열로 대체하도록 지시합니다. 마지막으로 변경된 문자열을 출력해보면 "안녕"이라는 단어에서 모음이 삭제된 것을 확인할 수 있습니다.

### 딥 다이브
위에서 사용한 `replacingOccurrences()` 메서드에 대해 더 깊이 알아보겠습니다. 이 메서드는 `String` 클래스의 메서드로, 첫 번째 인자로는 대체할 문자열이나 문자열 패턴, 두 번째 인자로는 대체될 문자열, 세 번째 인자로는 옵션을 지정할 수 있으며, 마지막 인자로는 대체할 범위를 지정할 수 있습니다. 옵션에는 정규식을 사용할 수도 있으며, 범위를 지정하지 않으면 문자열 전체에서 대체 작업이 수행됩니다.

### 더 알아보기
이 외에도 문자열에서 특정 패턴을 삭제하는 다양한 방법이 존재하며, 가령 정규식을 사용하지 않고도 문자열을 조작할 수 있습니다. 관련된 다양한 정보를 아래의 링크에서 더 자세히 알아보세요.

## 참고
- [Swift 문자열 처리 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift 정규식 가이드 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID411)