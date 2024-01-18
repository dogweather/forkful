---
title:                "문자열에서 날짜 추출하기"
html_title:           "Swift: 문자열에서 날짜 추출하기"
simple_title:         "문자열에서 날짜 추출하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇을 하고 왜?
날짜를 문자열에서 구문 분석하는 것은 우리가 날짜를 사용하고 처리하는 방식을 좀 더 유연하게 만들어줍니다. 프로그래머들은 때때로 입력된 날짜 데이터를 분석하고 사용하기 쉬운 형식으로 변환해야할 때가 있기 때문에 이 작업을 수행합니다.

## 어떻게 할까요?
```Swift
let dateString = "2020-02-14"

// DateFormatter 인스턴스를 생성하고 설정
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

// 날짜 문자열을 날짜로 변환
let date = dateFormatter.date(from: dateString)

// 변환된 날짜를 출력
print(date) // 2020-02-14 00:00:00 +0000
```

## 깊게 들어가보기
- 역사적 배경: 날짜 구문 분석은 프로그래밍에서 매우 일반적인 작업으로, 초기 시스템에서 날짜를 사용하는 방법과 널리 사용되는 날짜 형식에 따라 달라졌습니다.
- 대안: Swift에서는 DateFormatter 외에도 Calendar와 DateComponents를 사용하여 날짜를 조작하고 형식을 지정할 수 있습니다.
- 구현 세부 사항: Swift는 ISO 8601 형식의 날짜를 기본적으로 지원하므로 날짜 문자열의 형식에 대한 지정 없이도 구문 분석이 가능합니다.

## 관련 자료
- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift by Sundell - Dates & Time in Swift](https://www.swiftbysundell.com/basics/dates-and-times/)
- [Hacking with Swift - Working with Dates in Swift](https://www.hackingwithswift.com/articles/158/how-to-work-with-dates-and-times-in-swift)