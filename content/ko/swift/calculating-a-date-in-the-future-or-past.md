---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Swift: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜 계산이란 미래 또는 과거의 특정 날짜를 계산하는 것을 말합니다. 프로그래머들이 이를 하는 이유는 앱이나 웹사이트에서 날짜를 자동으로 계산하여 사용자들에게 현재 시간을 보여주기 위해서입니다.

## 하는 방법:
```Swift
// 현재 날짜를 얻기 위한 Date 객체 생성
let currentDate = Date()

// 미래의 날짜 계산
let futureDate = Calendar.current.date(byAdding: .day, value: 1, to: currentDate)

// 과거의 날짜 계산
let pastDate = Calendar.current.date(byAdding: .day, value: -1, to: currentDate)

print("현재 날짜: \(currentDate)")
print("미래 날짜: \(futureDate)")
print("과거 날짜: \(pastDate)")
```

**결과:**
```
현재 날짜: 2021-03-17 16:00:21
미래 날짜: 2021-03-18 16:00:21
과거 날짜: 2021-03-16 16:00:21
```

## 깊게 들어가보기:
1. **역사적 배경:** 날짜 계산은 컴퓨팅의 초기부터 사용되어 왔습니다. 달력을 참조하여 시스템의 시간과 날짜를 계산하는 것은 컴퓨터 프로그래밍에서 중요한 역할을 합니다.

2. **대안:** Swift에서는 날짜를 계산하는 데에 있어 여러 가지 방법이 있습니다. 다른 옵션들로는 DateComponent, TimeInterval 등이 있습니다.

3. **구현 세부사항:** Swift에는 날짜 및 시간 조작을 위한 기본 내장 라이브러리인 Foundation이 존재합니다. Foundation에서는 Date, Calendar, DateFormatter 등의 클래스를 제공하여 날짜 계산을 쉽게 할 수 있도록 지원하고 있습니다.

## 관련 자료:
- [Swift Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Swift Official Site](https://swift.org/)
- [Raywenderlich: Working with Date and Time in Swift](https://www.raywenderlich.com/1416791-working-with-date-and-time-in-swift#toc-anchor-009)