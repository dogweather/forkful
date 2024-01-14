---
title:                "Swift: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 과정에 참여하는 이유는 무엇일까요? 날짜를 문자열로 변환하면 텍스트로 데이터를 표현할 수 있고, 이를 다시 숫자로 바꿀 때 필요합니다.

## 하는 법

날짜를 문자열로 변환하는 방법은 간단합니다. 먼저, `DateFormatter`를 이용하여 원하는 날짜 형식을 지정합니다. 다음으로 `string(from: Date)` 메서드를 사용하여 변환할 날짜를 지정하고, 문자열로 변환하여 출력합니다.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

let date = Date()

let stringDate = dateFormatter.string(from: date)

print(stringDate) // 2021-01-01
```

위의 예시에서는 `yyyy-MM-dd` 형식으로 날짜를 변환하였습니다. 날짜 형식은 다양하게 설정할 수 있으며, [여기](https://nsdateformatter.com/)에서 더 많은 형식을 확인할 수 있습니다.

## 딥 다이브

`DateFormatter`는 날짜를 문자열로 변환할 때 매우 유용한 클래스입니다. 이 클래스를 사용하여 원하는 날짜 형식에 맞게 데이터를 표현할 수 있습니다. 또한 `DateFormatter`를 사용하여 문자열로 된 날짜를 다시 숫자 형태로 변환할 수도 있습니다.

`Date` 클래스와 `DateFormatter` 클래스에 대해 더 자세히 알고 싶다면 [여기](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)에서 확인할 수 있습니다.

## 관련 자료

- [날짜를 문자열로 변환하는 다른 방법 알아보기](https://azusa.hatenablog.com/entry/2019/08/10/164026)
- [DateFormatter 클래스 공식 문서](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift 날짜 관련 유용한 팁 모음](https://useyourloaf.com/blog/swift-date-cheat-sheet/)