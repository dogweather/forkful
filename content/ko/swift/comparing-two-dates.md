---
title:    "Swift: 두 날짜 비교하기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# 왜?

두 날짜를 비교하는 데 참여해야 하는 이유는 무엇일까요? 이 블로그 포스트에서는 Swift 프로그래밍에서 날짜를 비교하는 것의 중요성과 유용성에 대해 알아보도록 하겠습니다.

## 어떻게

Swift에서 두 날짜를 비교하는 방법을 알아보겠습니다. 아래의 코드 블록을 참고해 주세요.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "MM/dd/yyyy"
let date1 = dateFormatter.date(from: "03/27/2021")
let date2 = dateFormatter.date(from: "04/01/2021")

if date1! < date2! {
  print("date1 is earlier than date2.")
} else if date1! > date2! {
  print("date2 is earlier than date1.")
} else {
  print("date1 and date2 are the same.")
}

// Output:
// date1 is earlier than date2.
```

위의 예시에서는 `DateFormatter`를 사용하여 두 날짜를 `MM/dd/yyyy` 형식으로 변환하고, `<` 연산자를 사용하여 두 날짜를 비교합니다. 이 외에도 `>` 연산자나 `==` 연산자를 사용할 수 있습니다. 또한 `Calendar`를 사용하여 더 복잡한 비교도 가능합니다.

## 깊게 들어가기

두 날짜를 비교하는 것은 다양한 상황에서 매우 유용합니다. 예를 들어, 사용자의 생년월일과 현재 날짜를 비교하여 만 나이를 계산하는 데 사용할 수 있습니다. 또는 예약 날짜와 현재 날짜를 비교하여 사용자가 즉시 예약할 수 있는지를 확인하는 데도 사용될 수 있습니다.

Swift에서 날짜를 비교하는 더 많은 방법과 유용한 팁들을 알아보려면 아래의 링크들을 참고해 주세요.

# 참고

- [Apple Developer Documentation - Working with Dates](https://developer.apple.com/documentation/foundation/date)
- [How to compare two dates in Swift](https://www.hackingwithswift.com/example-code/system/how-to-compare-two-dates-in-swift)
- [10 Tips to become a better Swift Developer](https://medium.com/swlh/10-tips-to-become-a-better-swift-developer-ad94928e7d28)