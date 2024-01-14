---
title:    "Swift: 텍스트 검색 및 교체"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# 왜

텍스트를 찾고 바꾸는데 왜 관심을 가질까요? 그 이유는 간단합니다. 컴퓨터 프로그래밍을 하면서 반복적이고 지루한 작업을 자동화하고 싶을 때가 있기 때문입니다. 텍스트를 찾고 바꾸는 것은 이를 가능하게 해주는 유용한 도구입니다.

## 방법

Swift를 사용하여 텍스트를 찾고 바꾸는 방법을 알아보겠습니다. 먼저, 다음과 같이 원본 문장을 정의합니다.

```Swift
let originalSentence = "오늘은 맑은 날씨입니다."
```

이제 `replacingOccurrences()` 메서드를 사용하여 "맑은"을 "흐린"으로 바꾸는 것을 시도해보겠습니다.

```Swift
let replacedSentence = originalSentence.replacingOccurrences(of: "맑은", with: "흐린")
print(replacedSentence)
```

출력 결과는 다음과 같을 것입니다.

```
오늘은 흐린 날씨입니다.
```

위 코드에서는 `originalSentence` 변수에 저장된 문장을 대상으로 `replacingOccurrences()` 메서드를 호출하고, 찾을 텍스트인 "맑은"을 찾아서 "흐린"으로 바꾼 다음, 결과를 `replacedSentence` 변수에 저장하고 출력합니다.

## 깊게 파헤치기

더 깊이 들어가 보겠습니다. `replacingOccurrences()` 메서드의 세부 구현을 살펴보면 다음과 같습니다.

```Swift
func replacingOccurrences(of target: String, with replacement: String) -> String
```

메서드는 앞서 설명한 대로 "찾을 텍스트"와 "바꿀 텍스트" 파라미터를 받아 앞서 정의한 `originalSentence` 문자열을 대상으로 찾은 텍스트를 모두 바꾼 후 해당 내용을 새로운 문자열로 반환하는 것을 볼 수 있습니다. 이를 통해 우리는 간단한 문장에서도 여러 곳의 텍스트를 한번에 바꿀 수 있고, 더 복잡한 프로그램에서도 텍스트를 자유롭게 바꿀 수 있음을 알 수 있습니다.

# 참고

위에서 살펴본 `replacingOccurrences()` 메서드 외에도 Swift에서는 여러 가지 방법으로 텍스트를 찾고 바꾸는 기능을 제공합니다. 아래 링크들을 참고해보세요.

- https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- https://www.hackingwithswift.com/articles/118/10-quick-swift-tips-fixing-swifts-string-handling-depression
- https://www.avanderlee.com/swift/string-replacing-swift/

# 참고자료

- https://www.hackingwithswift.com
- https://www.avanderlee.com
- https://docs.swift.org