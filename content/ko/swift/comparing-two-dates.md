---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

두 날짜를 비교하는 것은 날짜가 다른지 확인하는 것을 의미합니다. 프로그래머들은 일반적으로 이벤트의 순서를 결정하거나 시간이 지난 후 어떤 작업을 수행해야 하는지 결정하기 위해 두 날짜를 비교합니다.

## 어떻게 하는가:

두 날짜를 비교하려면 Swift의 'compare' 기능을 사용합니다. 아래에서 코드 예시를 보실 수 있습니다:

```Swift 
import Foundation

let now = Date()
let laterDate = now.addingTimeInterval(5000.0) //adds approximately 1.4 hours

if now.compare(laterDate) == ComparisonResult.orderedAscending {
    print("laterDate happens after now")
} else {
    print("laterDate is either equal to or before now")
}
```

위 코드는 현재 시간을 now로 정의하고, 5000초 후의 시간을 laterDate로 정의합니다. 그리고 'compare' 함수를 사용하여 now가 laterDate보다 이전인지 확인합니다.

## Deep Dive:

비교 연산자는 Swift가 2014년에 처음 발표된 이후 버전이 업데이트되면서 세분화되고 개선되었습니다. 'compare' 기능 외에도 '이전(<)', '이후(>)', '이전 또는 동일(<=)', '이후 또는 동일(>=)'라는 비교 연산자를 사용할 수 있습니다. 날짜를 생성한 후 해당 날짜의 달, 일, 년, 시, 분, 초 등을 비교하여 더 세부적인 비교를 할 수도 있습니다.

## 참고자료:

- [Swift API Reference](https://swift.org/documentation/)
- [Swift's Handling of Dates](http://seventhsoulmountain.blogspot.com/2017/09/swift-date-handling.html)