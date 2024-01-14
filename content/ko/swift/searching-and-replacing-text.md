---
title:    "Swift: 텍스트 찾기와 교체하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜

텍스트를 검색하고 대체하는 것이 왜 필요한지 이유를 알아보겠습니다. 

일례로, 프로그래밍에 익숙하지 않은 사용자가 여러 파일에서 일일이 이름을 변경하려고 할 때, 모두 찾아서 일일이 바꾸는 것은 매우 번거로운 일일 것입니다. 따라서 텍스트를 검색하고 대체하는 기능은 시간과 노력을 절약하는 데 큰 도움이 됩니다.

# 어떻게

먼저, 검색 할 대상 텍스트를 지정합니다. 그리고 원하는 텍스트를 찾아서 대체하면 됩니다. 예를 들어, 아래 코드를 참고해보세요.

```Swift
let originalString = "안녕, Swift"
let replacedString = originalString.replacingOccurrences(of: "안녕", with: "Hello") 
print(replacedString) // 출력: Hello, Swift
```

replacingOccurrences 함수를 사용하여 "안녕"을 "Hello"로 대체했습니다. 이렇게 간단하게 텍스트를 검색하고 대체할 수 있습니다.

# 딥 다이브

이제 딥 다이브를 해보겠습니다. Swift에서는 기본적으로 String type에 대해 표준 라이브러리로 문자열을 처리하기 때문에, 텍스트를 검색하고 대체하는 기능도 쉽게 사용할 수 있습니다. 또한 대소문자를 구분하지 않고 검색하는 옵션도 제공하므로 원하는 대상을 놓치지 않고 검색할 수 있습니다.

여러 문자열을 동시에 대체하려면 복수의 텍스트를 찾아 대체할 수 있는 replaceOccurrences 함수를 사용하면 됩니다.

# 참고

- Swift 공식 문서: [String.replacingOccurrences](https://developer.apple.com/documentation/swift/string/2920221-replacingoccurrences)
- Hacking with Swift: [Replacing text in a string using replacingOccurrences(of:)](https://www.hackingwithswift.com/example-code/strings/replacing-text-in-a-string-using-replacingoccurrences-of)
- Baks Coding: [Swift 문자열 검색 및 대체](https://www.bakscoding.com/2018/12/swift_5.html)