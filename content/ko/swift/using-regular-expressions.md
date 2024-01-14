---
title:                "Swift: 정규 표현식을 사용하는 방법"
simple_title:         "정규 표현식을 사용하는 방법"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜 정규 표현식을 사용해야 할까요?

정규 표현식은 문자열에서 패턴을 찾아내고 대체 또는 추출하는 데 유용한 도구입니다. 예를 들어, 이메일 주소나 전화번호, 우편번호 등의 형식을 검증하거나 특정 단어나 문구를 찾는 등의 작업에 쉽게 사용할 수 있습니다.

# 사용 방법

정규 표현식을 사용하는 첫 번째 단계는 패턴을 정의하는 것입니다. 우선 `$`로 시작하고 `$`로 끝나는 정규 표현식 패턴 문자열을 생성합니다. 이 때, 패턴에 맞는 문자열이 있는지 찾아보고 싶은 원본 문자열을 입력합니다.

```Swift
let pattern = "^A\\d{2,3}$"
let original = "A123"

let result = original.range(of: pattern, options: .regularExpression)
if let range = result {
    print("\(original)는 정규 표현식 패턴 \(pattern)에 맞습니다.")
} else {
    print("\(original)는 정규 표현식 패턴 \(pattern)에 맞지 않습니다.")
}

//result: A123는 정규 표현식 패턴 ^A\\d{2,3}$에 맞습니다.
```

위 예시에서는 `^`로 시작하고 `\`를 이용해서 `\d`는 숫자를, `{}`는 개수를 정의합니다. 따라서 `A`로 시작하고 숫자 2~3개가 있는 문자열에 매칭됩니다.

# 자세히 알아보기

만약 정규 표현식을 자세히 공부하고 싶다면, 다음 사이트를 참고해보세요.
- [Swift에서 정규 표현식 사용하기](https://zeddios.tistory.com/325)
- [정규 표현식을 사용한 문자열 작업하는 방법](https://learnappmaking.com/regex-swift-how-to)

# 관련 자료

- [Regular Expressions in Swift - WWDC 2016](https://developer.apple.com/videos/play/wwdc2016/408/)
- [Swift by Sundell - Regular expressions in Swift](https://www.swiftbysundell.com/articles/regular-expressions-in-swift/)
- [Regex in Swift with NSRegularExpression](https://www.appcoda.com/swift-regular-expression/)