---
title:                "Swift: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오기 위해 Swift 프로그래밍을 할 수 있는 이유는 다양합니다. 예를 들어, 날짜를 간편하게 표시하거나 특정 작업을 특정 날짜와 비교하기 위해 사용할 수 있습니다. Swift에서 현재 날짜를 가져오는 방법을 알고 있으면 유용한 프로그래밍 능력을 갖추게 될 것입니다.

## 어떻게

현재 날짜를 가져오는 과정은 매우 간단합니다. 먼저 Date 타입의 변수를 선언하고 Date() 함수를 사용하여 현재 날짜를 초기화합니다. 그런 다음, DateFormatter를 사용하여 날짜를 원하는 형식으로 변환할 수 있습니다. 아래는 현재 날짜를 YYYY-MM-dd 형식으로 표시하는 예시 코드입니다.

```swift
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "YYYY-MM-dd"
let formattedDate = dateFormatter.string(from: currentDate)
print(formattedDate)
```

위 코드를 실행하면 현재 날짜가 "2021-07-15"와 같은 형식으로 출력됩니다.

## 딥 다이브

Date 타입에 대해 더 알아보고 싶다면, Apple의 공식 문서를 참조하는 것이 좋습니다. Date 타입은 사용자의 장치 설정과 시간대를 고려하여 데이터를 저장하고 비교하는데 사용됩니다. 또한 DateComponents를 사용하여 시간 간격을 계산하거나 날짜와 시간을 조작할 수도 있습니다.

만약 현재 날짜 대신 다른 날짜를 가져오려면 DateComponents와 Calendar를 사용하면 됩니다. 또한, TimeInterval을 사용하여 시간 간격을 나타내는 데 사용할 수도 있습니다.

## 관련 링크

- [Apple 공식 문서](https://developer.apple.com/documentation/foundation/date)
- [Date Formatter 사용하기](https://www.swiftdevcenter.com/date-formatter-swift/)
- [DateComponents와 Calendar 사용하기](https://www.codegrepper.com/code-examples/swift/swift+get+current+date+and+time)