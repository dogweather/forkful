---
title:                "문자열 추출하기"
html_title:           "Swift: 문자열 추출하기"
simple_title:         "문자열 추출하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜 
우리는 프로그래밍을 할 때, 문자열을 다루는 경우가 많습니다. 그리고 때때로, 우리는 문자열에서 원하는 부분만 추출해야 할 때가 있습니다. 이때, **서브스트링 추출**을 사용하면 유용합니다. 

## 사용 방법 
서브스트링을 추출하는 방법은 간단합니다. 먼저 추출할 문자열을 `String` 변수에 저장합니다. 그 다음, `substring` 메소드를 사용하여 시작 지점과 길이를 지정해주면 됩니다. 아래의 예시를 살펴보세요.

```Swift
let myString = "Hello, world!"
let mySubstring = myString.substring(from: 7, length: 5)

print(mySubstring) // output: world
```

위의 예시에서는 `substring` 메소드를 사용하여 `"world"`라는 서브스트링을 추출했습니다. `from` 매개변수는 시작 지점을 나타내고, `length` 매개변수는 추출할 문자열의 길이를 나타냅니다. 

## 깊게 들어가보기 
서브스트링 추출은 `substring` 메소드 외에도 다양한 방법으로 사용할 수 있습니다. 예를 들어, `prefix` 메소드를 사용하면 문자열의 맨 앞부터 지정한 길이만큼의 서브스트링을 추출할 수 있고, `suffix` 메소드를 사용하면 문자열의 맨 뒤부터 지정한 길이만큼의 서브스트링을 추출할 수 있습니다.

또한, 서브스트링에서도 다양한 기능을 사용할 수 있습니다. 예를 들어, `contains` 메소드를 사용하면 해당 서브스트링이 원본 문자열에 포함되어 있는지 확인할 수 있고, `replacingOccurrences` 메소드를 사용하면 특정 문자열을 다른 문자열로 대체할 수 있습니다. 

## 더 알아보기 
- [Apple 공식 문서 - String](https://developer.apple.com/documentation/swift/string)
- [Swift Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [String Cheat Sheet](https://goshdarnclosures.com/2019/08/23/string-cheat-sheet.html)

## 참고 자료 
- [Swift에서 문자열 다루기](https://blog.naver.com/ksg5141/221387136067)
- [Swift의 String을 슬라이싱하거나 문자열에서 부분 추출하는 방법](https://m.blog.naver.com/PostView.nhn?blogId=naemin1127&logNo=220290913436&proxyReferer=https:%2F%2Fwww.google.com%2F)

---

*번역: 이 글은 Swift에서 문자열에서 서브스트링을 추출하는 방법에 대해 알아봤습니다. 문자열 다루는 기능을 더 알아보고 싶다면 위의 링크를 참고하세요!*