---
title:    "Swift: 미래나 과거의 날짜 계산하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 계산하는 것은 우리 일상에서 매우 중요합니다. 우리는 앞으로나 과거의 일정을 계획하거나, 특정 날짜까지의 남은 시간을 알고 싶을 때 자주 날짜 계산을 하게 됩니다. 이번 포스트에서는 Swift를 사용해서 미래나 과거의 날짜를 계산하는 방법을 알아보겠습니다.

## 어떻게

날짜를 계산하기 위해서는 `DateComponents`와 `Calendar` 클래스를 사용해야 합니다. 먼저, 원하는 날짜를 `DateComponents` 객체로 만들어줍니다. 그리고 해당 날짜의 `Calendar` 객체를 생성한 후, `date(byAdding: DateComponents, to: Date)` 메서드를 사용하여 계산하고 싶은 날짜를 얻을 수 있습니다.

예시 코드는 아래와 같습니다.

```Swift
let today = Date()

var futureDateComponents = DateComponents()
futureDateComponents.day = 30

let calendar = Calendar.current

if let futureDate = calendar.date(byAdding: futureDateComponents, to: today) {
    print("오늘부터 30일 이후의 날짜는 \(futureDate)입니다.")
}
```

위의 코드를 실행하면, 현재 날짜로부터 30일 이후의 날짜가 출력될 것입니다.

```Swift
오늘부터 30일 이후의 날짜는 2021-09-04 12:00:00 +0000입니다.
```

마찬가지로, 과거의 날짜를 계산하고 싶다면 `DateComponents`의 값을 음수로 설정해주면 됩니다.

## 깊이 들어가기

날짜를 계산하는 데에는 다양한 요인들이 포함될 수 있습니다. 예를 들어, 특정 날짜의 요일을 알고 싶다면 `Calendar`의 `component(_: from: Date)` 메서드를 사용할 수 있고, 두 날짜 사이의 차이를 알고 싶다면 `dateComponents(_:from:to:)` 메서드를 사용할 수 있습니다.

또한, `Calendar` 클래스에는 다양한 옵션들이 있어서 원하는 날짜의 정보를 더욱 세부적으로 가져올 수도 있습니다. 참고하시기 바랍니다.

## 참고

- [Apple Document - Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Stack Overflow - Calculating future/past dates in Swift](https://stackoverflow.com/questions/26623059/calculating-future-past-dates-in-swift)
- [Swift by Sundell - Working with dates in Swift](https://www.swiftbysundell.com/basics/working-with-dates-in-swift/)