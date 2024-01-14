---
title:    "Swift: 미래나 과거의 날짜를 계산하는 법"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜
미래 또는 과거의 날짜를 계산하는 것이 왜 중요한지 알고 계신가요? 예측할 수 없는 이벤트나 업무를 위해 미래의 날짜를 알려면, 자동으로 계산하는 것이 바람직합니다. 또한 인터넷 응용 프로그램에서 특정 날짜를 계산하는 데 이러한 기능이 널리 사용되고 있습니다.

## 하는 법
미래 또는 과거의 날짜를 계산하려면, Swift에서 기본적으로 제공되는 날짜 및 시간 관련 클래스를 사용합니다. 아래의 코드는 현재 날짜로부터 3일 이후의 날짜를 계산하는 예제입니다.

```Swift
let currentDate = Date()
let futureDate = Calendar.current.date(byAdding: .day, value: 3, to: currentDate)
print(futureDate!) 
```
> 출력: 2019년 8월 21일 오후 1시 10분 17초

위의 예제에서 `date(byAdding:to:)` 메소드를 사용하여 현재 날짜에 3일을 더한 날짜를 계산하였습니다. 이외에도 `year`, `month`, `hour` 등의 값도 계산할 수 있으며, 계산하는 값을 조정하여 미래 또는 과거의 날짜를 얻을 수 있습니다.

## 심층 탐구
날짜와 시간을 계산하는 데는 다양한 방법이 있습니다. Swift에서는 기본적으로 Gregorian 캘린더를 사용하지만 다른 캘린더도 사용할 수 있습니다. 또한 사용하는 장치의 타임존에 따라 날짜가 달라질 수 있으므로, 타임존의 관리도 중요합니다. 또한 윤년이나 다른 국가의 언어에 따라 다른 결과를 얻을 수 있으므로, 정확한 계산을 위해서는 그에 따른 코드를 작성해야 합니다.

## 참고
- [Swift날짜 및 캘린더 관련 클래스 문서](https://developer.apple.com/documentation/foundation/date)
- [NSDate와 Swift에서의 날짜 다루기](https://zeddios.tistory.com/72)
- [타임존과 타임 캘린더 관리하기](https://zeddios.tistory.com/73)
- [Swift와 캘린더 계산](https://ios-development.tistory.com/5)