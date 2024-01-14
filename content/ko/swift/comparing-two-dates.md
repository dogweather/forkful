---
title:                "Swift: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜
날짜 비교를 하고 싶은 이유는 무엇일까요? 어떤 경우에 이 기법이 유용할까요? 다음 내용에서 예제와 함께 알아보도록 하겠습니다.

## 어떻게
Swift에서 두 날짜를 비교하는 방법은 다양합니다. 가장 간단한 방법은 "date1 > date2"와 같은 비교 연산자를 사용하는 것입니다. 또는 "compare(_:_:)" 메서드를 사용하여 두 날짜를 비교할 수 있습니다. 예제 코드를 통해 살펴보도록 하겠습니다.

```Swift
let date1 = Date().addingTimeInterval(3600) // 현재 시간보다 1시간 뒤 날짜
let date2 = Date() // 현재 시간

if date1 > date2 {
    print("\(date1)은 \(date2)보다 미래입니다.")
} else {
    print("\(date1)은 \(date2)과 같거나 과거입니다.")
}

/* Output:
 2019-10-13 12:00:00 +0000은 2019-10-13 11:00:00 +0000보다 미래입니다.
*/
```
위의 예제 코드에서는 현재 시간보다 1시간 뒤인 날짜와 현재 시간을 비교하고 있습니다. 만약 해당 조건에 맞지 않는다면, 반대로 모든 날짜는 같거나 과거라는 의미입니다.

```Swift
let date1 = Date() // 현재 시간
let date2 = Date().addingTimeInterval(-3600) // 현재 시간보다 1시간 빠른 날짜

switch date1.compare(date2) {
case .orderedAscending:
    print("\(date1)은 \(date2)보다 미래입니다.")
case .orderedSame:
    print("\(date1)과 \(date2)은 같은 날짜입니다.")
case .orderedDescending:
    print("\(date1)은 \(date2)보다 과거입니다.")
}

/* Output:
 2019-10-13 12:00:00 +0000과 2019-10-13 11:00:00 +0000은 같은 날짜입니다.
*/
```
위의 예제 코드는 "compare(_:_:)" 메서드를 사용하여 두 날짜를 비교하고 있습니다. 이 메서드는 두 날짜를 비교한 결과를 "ComparisonResult" 타입으로 반환합니다. 그리고 해당 값에 따라 다른 동작을 수행할 수 있습니다.

## 깊이 파고들기
Swift에서 날짜를 비교하는 방법은 매우 다양합니다. 위에서 소개한 방법 외에도 "Calendar"나 "DateComponents"를 사용하여 비교하는 방법도 있습니다. 또한 날짜 비교를 위한 추가적인 옵션도 존재합니다. 정확한 비교를 위해서는 해당 날짜가 어느 시간대에 속하는지, 혹은 어떤 달력을 사용하는지 등에 대해 고려해야 합니다.

# 참고
- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Swift Docs - Basic Operators](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)
- [Swift Docs - Date and Time Calculations](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)