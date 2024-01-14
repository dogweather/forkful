---
title:    "Swift: 날짜를 문자열로 변환하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 이유에 대한 이유는 간단합니다. 우리는 프로그래밍에서 날짜와 시간을 자주 다루기 때문입니다. 우리는 날짜와 시간을 가지고 있지만, 우리는 문자열로 표현하고 싶습니다. 이러한 작업은 여러 가지 이유로 인해 필요할 수 있습니다. 예를 들어, 사용자가 이해하기 쉬운 형식으로 날짜를 표시하고 싶을 수 있습니다. 또는 데이터베이스에 날짜를 저장하고 싶을 수도 있습니다. 어떤 이유로 하더라도, 날짜를 문자열로 변환하는 것은 매우 유용합니다.

## 하는 방법

우리는 Swift에서 `dateToString()` 함수를 사용하여 날짜를 문자열로 변환할 수 있습니다. 예를 들어, 우리가 현재 날짜를 나타내는 `Date` 객체를 가지고 있다고 가정해 봅시다. 이것을 `date`라는 변수에 할당하겠습니다. 그리고 이를 문자열로 변환하고 싶다면 다음과 같이 하면 됩니다.

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd" // 날짜 형식 설정
let dateAsString = formatter.string(from: date)
print(dateAsString) // 2019-11-05 출력
```

우리는 `DateFormatter` 클래스의 인스턴스를 만들고, `dateFormat` 속성을 이용하여 날짜 형식을 설정합니다. 그리고 `string(from:)` 메소드를 사용하여 `Date` 객체를 문자열로 변환합니다. 이렇게 하면 우리는 "2019-11-05"와 같은 형식의 문자열을 얻을 수 있습니다.

## 깊이 있는 내용

`DateFormatter` 클래스에 대해 더 많은 깊이 있는 정보를 알아보겠습니다. 우리는 날짜 형식을 `dateFormat` 속성을 이용하여 `yyyy-MM-dd`와 같이 설정했지만, 실제로 이 속성은 좀 더 복잡합니다. 여러분은 다양한 방식으로 날짜 형식을 설정할 수 있습니다. 예를 들어, "yyyy년 MMMM dd일 (a)"와 같이 설정하면 "2019년 11월 05일 (화)"와 같이 출력됩니다. 이렇게 날짜 형식을 설정하는 것은 여러분의 필요에 맞춰서 유연하게 할 수 있습니다.

또한, `DateFormatter` 클래스는 지역화 (Localization) 기능도 제공합니다. 즉, 각 나라의 언어에 맞게 날짜를 표시할 수 있습니다. 기본적으로는 시스템의 로케일에 따라 설정되지만, 여러분은 이를 직접 설정할 수도 있습니다.

## 참고자료

- [Apple 공식 문서 - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift 공식 문서 - Date](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID310)
- [Swift의 날짜 형식 만들기 블로그 포스트](https://mobidevtalk.com/ios-swift-date-format-201909012)