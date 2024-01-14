---
title:    "Swift: 현재 날짜 받기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜 날짜를 얻는 걸까?

날짜는 프로그래밍에서 매우 중요한 개념입니다. 예를 들어, 어떤 응용 프로그램은 오늘의 날짜를 표시하거나 날짜에 따라서 어떤 동작을 수행하는 경우가 있습니다. 따라서 현재 날짜를 얻는 것은 매우 유용하며 많은 프로그래머가 필요로 합니다.

## 어떻게 구할 수 있을까?

아래 예제 코드를 참고하여 현재 날짜를 구하는 방법을 알아보겠습니다.

```Swift
let currentDate = Date()
print(currentDate)
```

위 코드를 실행하면 현재 시간과 날짜가 출력됩니다.

```Swift
// 현재 날짜와 시간 출력
2021-09-25 15:23:45 +0000
```

만약 날짜를 다른 포맷으로 출력하고 싶다면 `DateFormatter`를 사용하면 됩니다. 예를 들어, `yyyy-MM-dd` 포맷으로 날짜를 출력하려면 아래와 같이 코드를 작성하면 됩니다.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let formattedDate = dateFormatter.string(from: currentDate)
print(formattedDate)
```

```Swift
// yyyy-MM-dd 포맷으로 날짜 출력
2021-09-25
```

## 더 깊게 알아보기

`Date()`는 기본적으로 현재 시간과 날짜를 가져옵니다. 하지만, 옵션 파라미터를 사용하여 원하는 날짜와 시간을 가져올 수도 있습니다. 예를 들면 아래와 같은 방법입니다.

```Swift
// 현재 시간에 30초를 더하여 출력하기
let currentDate = Date()
let futureDate = Calendar.current.date(byAdding: .second, value: 30, to: currentDate)
print(futureDate)
```

```Swift
// 30초 뒤의 날짜 출력
2021-09-25 15:24:15 +0000
```

Swift에서는 `Date()`을 사용하여 현재 날짜와 시간을 가져오는 것이 매우 간단하고 쉽습니다. 하지만, 날짜와 시간을 다루는 방법에 대해서는 더욱 많은 옵션과 기능이 있으니 관심이 있다면 더 깊이 공부해보시기 바랍니다.

## 더 알아보기

[날짜와 시간 다루기](https://kr.includehelp.com/objective-c/how-to-get-current-date-and-time-in-objective-c.aspx) <br> [Swift Date, Calendar, DateComponents 사용해보기](https://valley1985.tistory.com/32) <br> [날짜와 시간 다루기 레퍼런스 문서](https://developer.apple.com/documentation/foundation/dates)