---
title:                "날짜를 문자열로 변환하기"
html_title:           "Swift: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 문자열로 변환하는 것은 프로그래머들이 날짜와 시간 정보를 표현하고 조작하는 데 도움이 됩니다. 이를 통해 사용자들은 쉽게 읽고 이해할 수 있는 형식으로 날짜를 표시할 수 있습니다.

## 어떻게:
```Swift
let date = Date() //현재 날짜와 시간
let dateFormatter = DateFormatter()
// DateFormatter 객체를 생성하여 날짜를 원하는 형식에 맞게 변환할 준비를 합니다.

dateFormatter.dateFormat = "EEEE, MMM dd, yyyy"
// 알파벳 'E'는 요일, 알파벳 'M'은 월, 숫자 'd'는 일, 그리고 숫자 'y'는 년도를 나타냅니다. 이렇게 DateFormat을 설정하면 날짜가 다음과 같이 표시됩니다.
// "Friday, Aug 13, 2021"

let stringDate = dateFormatter.string(from: date)
print(stringDate) // "Friday, Aug 13, 2021"
```

## 깊게 파헤치기:
날짜를 문자열로 변환하는 방법은 오랜 역사를 가지고 있습니다. 예를 들어 과거에는 매우 복잡한 방식으로 날짜를 표현했지만 지금은 훨씬 간단하고 유연한 방식으로 가능합니다. 또한 다양한 프로그래밍 언어에서도 비슷한 기능을 제공합니다. Swift에서는 DateFormatter를 사용하여 날짜를 원하는 형식에 맞게 변환할 수 있습니다. 또한 다른 방법으로는 날짜와 관련된 라이브러리를 사용하는 것이 있습니다.

## 같이 보기:
- [NSDateFormatter](https://developer.apple.com/documentation/foundation/nsdateformatter)
- [날짜 값 다루기 - Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/DatesAndTimes.html)