---
title:                "Swift: 텍스트 검색 및 대치"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
야외활동을 계획하고있는 이들은 스위프트 프로그래밍을 사용하여 직접 친구와 사진을 찍고 공유하기 위해 이 기능을 사용할 수 있습니다.

## 사용 방법 

우리는 다음과 같이 텍스트를 검색하여 이를 대체할 수 있습니다.:
```Swift
let originalString = "나는 야외 활동을 좋아해!"
let replacedString = originalString.replacingOccurrences(of: "야외", with: "실내")
print(replacedString)
```
아래는 위 코드의 출력 결과입니다.:
```
나는 실내 활동을 좋아해!
```

## 깊이 파고들기

"replacingOccurrences" 메서드는 특정 문자열을 대체하는 데 사용될 수 있습니다. 이 메서드는 대체 시 특정 옵션들을 설정할 수도 있습니다. 예를 들어, "caseSensitive" 옵션을 사용하여 대소문자를 구분할 수 있습니다. 또는 "regularExpression" 옵션을 통해 정규표현식을 활용하여 특정 패턴을 대체할 수도 있습니다.

## 관련 링크

- [Swift 공식 문서 - Replacing Substrings](https://developer.apple.com/documentation/swift/string/2965621-replacingoccurrences)
- [Swift DocC - String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift 문자열 처리 관련 블로그 포스트](https://medium.com/@EricaSwift/strings-in-swift-strings-characterstring-nsstring-and-more-95cf2e5345c0)

## 더 알아보기

이 포스트에서는 스위프트의 깊은 내부를 더 알아보지는 않았지만, 문자열 검색 및 대체에 대해 배우는 것은 실생활에서 많은 도움이 될 것입니다. 다음 링크들을 통해 더 많은 스위프트 프로그래밍 기법을 배울 수 있으며, 새로운 앱 또는 프로젝트를 만들어보면서 이러한 기술들을 적용해보세요.

## 관련 자료

- [Swift와 Xcode로 iOS 앱 개발하기](https://www.educba.com/swift-tutorial/)
- [스위프트 프로그래밍 언어 문서](https://docs.swift.org/swift-book/)
- [iOS 앱 개발을 위한 스위프트 입문](https://www.udemy.com/course/ios-starter-kit/)