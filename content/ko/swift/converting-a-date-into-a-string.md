---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 필요한가? 

날짜를 문자열로 변환하는 것은 정해진 형식의 문자열로 날짜를 표현하는 작업입니다. 이를 통해 개발자들은 원하는 형식으로 날짜와 시간을 쉽게 표현하고, 사용자와의 상호작용을 더욱 풍부하게 할 수 있습니다. 

## 어떻게 해야하는가?

Swift의 DateFormatter 클래스를 사용하면 간단히 해결할 수 있습니다. 다음은 사용 예제와 결과 출력입니다.

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()

formatter.dateStyle = .short
let shortDate = formatter.string(from: date)
print(shortDate)
```

위 코드의 실행 결과는 시스템 설정에 따라 다를 수 있지만, 일반적을 예를 들면 '11/3/21'와 같이 출력됩니다.

## 심화 학습

1) 날짜를 문자열로 변환하는 작업은 프로그래밍의 초창기부터 있었던 작업입니다. 기계와 사람이 이해할 수 있는 방식 간의 간극을 줄이기 위한 방법이죠.

2) Swift 외에도 JavaScript, Python, Java 등 다양한 프로그래밍 언어에서 날짜를 문자열로 변환하는 방법을 제공하고 있습니다. 각 언어에 대한 사용법을 따로 알아보는 것이 좋습니다.

3) Swift의 'DateFormatter'는 국제 표준에 따라 날짜와 시간 정보를 다룹니다. 사용자의 지역과 언어 설정에 따라 결과값이 다르게 출력될 수 있습니다.

## 참고 자료

1) [Apple Developer Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
   
2) [Stack Overflow: Convert Date to String format in Swift](https://stackoverflow.com/questions/35700281/date-format-in-swift)

위 두 링크로 가면 Swift에서 날짜를 문자열로 변환하는 방법에 대해 더 자세한 정보와 예제를 볼 수 있습니다. 이를 바탕으로 더 많은 학습을 이어가보세요.