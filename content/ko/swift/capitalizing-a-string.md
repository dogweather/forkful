---
title:                "Swift: 스트링 대문자로 변환하기"
simple_title:         "스트링 대문자로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜?

문자열을 대문자로 변환하는 데는 왜 참여해야 할까요? 매우 간단한 작업일 수 있지만 유용한 기능입니다.

## 사용 방법

```Swift
let word = "hello"
let capitalizedWord = word.capitalized
print(capitalizedWord)

// Output: "Hello"
```

위의 예제 코드에서 볼 수 있듯이 `capitalized` 메소드를 사용하면 문자열의 첫 글자를 대문자로 변환할 수 있습니다. 이외에도 `uppercased` 메소드를 사용하면 문자열 전체를 대문자로 변환할 수 있습니다. 

```Swift
let sentence = "i love swift"
let uppercasedSentence = sentence.uppercased
print(uppercasedSentence)

// Output: "I LOVE SWIFT"
```

## 깊게 들어가기

`capitalized`나 `uppercased` 메소드는 문자열을 변환할 때 유니코드에 기반하며, 문자열의 언어 설정에 따라 처리가 조금씩 다를 수 있습니다. 또한 특수문자나 공백도 정확히 다루어야 하는데, 이를 위해서는 `capitalized(with:)` 혹은 `uppercased(with:)` 메소드를 사용할 수 있습니다. 

## See Also

[Swift 문자열 관련 공식 문서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)

[Swift 문자열 대소문자 변환 관련 포스트](https://learnxinyminutes.com/docs/ko-kr/swift-kr/)