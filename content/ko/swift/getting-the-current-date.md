---
title:    "Swift: 현재 날짜 가져오기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜 오늘 날짜를 얻는 것이 중요한가요?

오늘 날짜를 얻는 것은 프로그래밍에서 매우 중요합니다. 왜냐하면 많은 애플리케이션에서 현재 날짜를 사용하여 작업을 처리하기 때문입니다. 예를 들어, 예약 시스템이나 일정 관리 앱에서는 현재 날짜를 사용해 다가오는 예약이나 일정을 확인할 수 있습니다.

# 어떻게 현재 날짜를 얻을 수 있나요?

```Swift
let now = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
let currentDate = formatter.string(from: now)
print(currentDate)
```

위의 코드는 Swift에서 현재 날짜를 얻는 간단한 예시입니다. Date()를 호출하여 현재 시간을 가져오고, DateFormatter를 사용하여 원하는 날짜 형식으로 포맷팅한 후 string(from:) 메소드를 호출하여 문자열로 변환합니다. 여기서는 "yyyy-MM-dd" 형식으로 변환하고, 출력 결과는 "2021-08-04"와 같이 나타납니다.

# 더 깊이 파헤쳐보기

Swift에서는 Date 객체를 사용하여 다양한 시간과 날짜 정보를 가져올 수 있습니다. 예를 들어, Date 객체의 components 메소드를 사용하면 년, 월, 일, 시간 등의 세부 정보를 가져올 수 있습니다. 또한 추가적인 날짜 계산을 위해 Calendar 클래스를 사용할 수 있습니다. 자세한 내용은 [Apple 공식 문서](https://developer.apple.com/documentation/foundation/date)를 참고하시기 바랍니다.

# 더 많은 학습 자료

[날짜와 시간을 다루는 방법에 대한 더 자세한 내용](https://cocoacasts.com/how-to-work-with-dates-and-times-in-swift)을 알고 싶다면 여기를 클릭해주세요. 또한 [Swift Dev Center](https://developer.apple.com/swift/)에서 Swift 프로그래밍에 대한 다양한 학습 자료를 찾아볼 수 있습니다.

# 더 찾아보기

[Swift Foundation 프레임워크](https://developer.apple.com/documentation/foundation)를 통해 날짜와 시간을 다루는 방법뿐만 아니라 많은 다른 유용한 기능을 찾아볼 수 있습니다. [Swift Language Guide](https://docs.swift.org/swift-book/)에서도 더 많은 내용을 학습할 수 있습니다.