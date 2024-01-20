---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜필요한가?
문자열에서 날짜를 파싱하는 것은 번호 및 문자를 사용해 특정 날짜로 변환하는 프로세스입니다. 이로써 프로그래머는 문자열 데이터를 조작하고 활용할 수 있게 됩니다.

## 어떻게 하는가:
Swift에서 문자열 날짜를 파싱하려면 DateFormatter를 사용합니다.

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let date = dateFormatter.date(from: "2021-07-15")!

print(date)
```

이 코드를 실행하면 "2021-07-15” 문자열이 Date 타입으로 파싱되어 출력됩니다.

## 디퍼다이브:
날짜 파싱은 오래 전부터 존재하는 과정으로, 용도에 따라 여러 방식으로 구현되곤 했습니다. Swift에서는 날짜 표준 형식을 사용하여 문자열을 날짜로 변환할 수 있으나 다양한 날짜 포맷에도 대응하도록 커스텀 형식을 설정할 수 있습니다.

Swift 외의 다른 프로그래밍 언어에서도 이와 유사한 기능을 제공하며, 대표적으로 Java의 SimpleDateFormat, Python의 datetime 등이 있습니다.

Swift의 DateFormatter는 유연한 날짜 파싱을 가능하게 하지만, 사용간에는 올바른 형식 지정 문자열을 설정해야 하며, 특히 July 와 7월과 같이 지역에 따라 다른 문자열을 적절하게 처리해야 합니다.

## 참고자료:
1. [Apple Developer Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
3. [Formatting Dates in Swift](https://www.hackingwithswift.com/articles/141/working-with-dates-in-swift)