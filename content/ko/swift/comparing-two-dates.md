---
title:                "두 날짜 비교하기"
html_title:           "Swift: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 비교하는 것은 무엇인가요? 프로그래머들이 왜 이것을 할까요? 
날짜를 비교한다는 것은, 두 개의 날짜를 비교하여 그 중 어느 날짜가 앞선 것인지를 판단하는 것을 말합니다. 
프로그래머들은 이를 자주 사용하는데, 예를 들어 어떤 이벤트가 특정 날짜 이후 일어나야 하는 경우 날짜를 비교하여 이를 구현할 수 있기 때문입니다.

## 방법:
```Swift

// 두 날짜 비교하기
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"

let date1 = formatter.date(from: "2020-01-01")!
let date2 = formatter.date(from: "2020-02-01")!

if date1 < date2 {
    print("date1 is earlier than date2")
} else if date1 > date2 {
    print("date1 is later than date2")
} else {
    print("date1 and date2 are the same")
}
// 출력: 날짜 1은 날짜 2보다 이른 날짜입니다.
```

## 깊이 파고들기:
(1) 날짜를 비교하는 것은 과거에도 많이 사용되었으며, 현재에도 많이 사용되는 기술입니다. (2) 날짜를 비교하는 다른 방법으로는 "compare" 함수를 사용하는 것과 "isBefore" 또는 "isAfter" 함수를 사용하는 것이 있습니다. (3) 날짜를 비교하는 방법에는 다양한 방법이 있지만, 현재 시간을 기준으로 비교하는 것이 일반적입니다.

## 또한 보기:
- [Swift의 DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [날짜와 시간 관련 문제 해결을 위한 Swift 지침](https://ericasadun.com/2018/03/12/dates-and-times-in-swift-cheat-sheet/)
- [Swift에서 날짜를 비교하는 방법](https://blog.karmadust.com/swift-date-comparison-tips/)