---
title:                "정규 표현식 사용하기"
html_title:           "Swift: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
자주 사용하는 정규식을 사용하는 이유는 간단합니다 - 텍스트에서 특정 문자열을 찾거나 대체할 수 있다는 유연성 때문입니다.

## 사용 방법
정규식을 사용하려면 ```Swift NSRegularExpression ``` 클래스를 사용하여 패턴을 만들고 ```Swift range(of:options:range:locale:)``` 메소드를 사용하여 텍스트에서 해당 패턴을 찾을 수 있습니다.

예를 들어, 만약 "apple"이라는 단어를 찾기 위해 정규식을 사용하려면 다음과 같이 코드를 작성할 수 있습니다:

```Swift
guard let regex = try? NSRegularExpression(pattern: "apple", options: [.caseInsensitive]) else {
  return
}

let text = "I'm craving for an Apple"

if let range = regex.range(of: text, options: [], range: NSRange(location: 0, length: text.characters.count), locale: nil) {
  // 찾은 문자열을 출력
  print(text.substring(with: range))
}
else {
  print("해당 단어를 찾을 수 없습니다.")
}
```

출력은 다음과 같을 것입니다:

`Apple`

## 딥 다이브
정규식은 강력한 도구이지만, 사용하기에는 다소 복잡할 수 있습니다. 예를 들어, 패턴이 너무 길 경우 ```Swift NSRegularExpression ``` 클래스에서 일부 제한 사항이 존재합니다. 또한, 오른쪽에서 왼쪽으로 읽는 지원을 제공하지 않기 때문에 특정 패턴을 찾는 경우에는 첫 번째 일치 항목을 찾지 못할 수 있습니다.

정규식을 사용하는 더 많은 방법에 대해서는 [Apple의 공식 문서](https://developer.apple.com/documentation/foundation/nsregularexpression)를 참조하시기 바랍니다.

## 관련 자료
[Swift 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID290)에서 문자열과 문자에 대한 더 많은 정보를 확인할 수 있습니다.

[Regular-Expressions.info](https://www.regular-expressions.info/)는 간단한 정규식 구문부터 복잡한 패턴에 대한 자세한 정보를 제공합니다.

[Regexr](https://regexr.com/)은 텍스트에서 정규식 패턴을 테스트하고 디버그할 수 있는 좋은 도구입니다.