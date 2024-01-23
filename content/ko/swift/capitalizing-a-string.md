---
title:                "문자열 대문자로 변환하기"
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
문자열 대문자화는 모든 문자를 대문자로 바꾸거나 첫 글자만 대문자로 만듭니다. 가독성을 높이거나, 정형화된 데이터 형식을 맞추기 위해 사용합니다.

## How to: (방법)
```Swift
let lowerCaseString = "안녕하세요, 여러분!"
let upperCasedString = lowerCaseString.uppercased() // 모든 문자를 대문자로
let capitalizedString = lowerCaseString.capitalized // 문장의 첫 글자만 대문자로

print(upperCasedString) // 출력: "안녕하세요, 여러분!"
print(capitalizedString) // 출력: "안녕하세요, 여러분!"
```

## Deep Dive (심층 분석)
옛날에 컴퓨터가 없던 시절, 대문자는 중요한 단어나 문장을 강조하기 위해 사용되었습니다. 프로그래밍에서는 이를 데이터 정규화의 한 형태로 사용하여, 예를 들어 사용자 입력이나 데이터베이스에서 일관된 문자 형식을 유지합니다.

대문자화에는 여러 방법이 있는데, `uppercased()`는 모든 문자를 대문자로 바꾸고, `capitalized`는 각 단어의 첫 글자만 대문자로 바꿉니다. 그 밖에 특정 조건에 따라 대문자화를 하는 방법도 있으며, 이는 사용자가 직접 함수를 만들어 조정할 수 있습니다.

Swift의 문자열 처리는 Unicode를 완벽하게 지원합니다, 즉, 다양한 언어와 기호에 대해서도 대문자화 작업을 정확하게 수행할 수 있음을 의미합니다. 그러나 주의할 점은, 특정 언어에서는 대문자와 소문자 개념이 없거나 다르게 작동할 수 있다는 것입니다.

## See Also (참고자료)
- Swift 문자열 관련 공식 문서: [String — Swift Documentation](https://developer.apple.com/documentation/swift/string)
- Unicode에 대한 더 많은 정보: [Unicode Standard](http://www.unicode.org/standard/standard.html)
