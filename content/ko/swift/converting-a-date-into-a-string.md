---
title:                "날짜를 문자열로 변환하기"
html_title:           "Swift: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것에 관심을 가지는 이유는 무엇일까요? 스위프트가 제공하는 다양한 기능들 중에서 날짜를 처리하는 것이 가장 흥미롭고 유용하기 때문입니다. 이 문서에서는 날짜를 문자열로 변환하는 방법과 이 과정에서 알아둬야 할 것들을 알려드리겠습니다.

## 어떻게

날짜를 문자열로 변환하는 방법은 간단합니다. ```String``` 데이터 타입의 ```init``` 메서드를 사용하면 됩니다. 그리고 날짜를 지정해줄 때 사용하는 ```DateFormatter``` 클래스의 인스턴스를 만들어주면 더욱 정확하게 원하는 형태로 날짜를 문자열로 바꿀 수 있습니다.

예를 들어, 2021년 10월 5일 12시 30분을 "yyyy년 MM월 dd일 HH시 mm분" 형식으로 표현하고 싶다면 다음과 같이 코드를 작성하면 됩니다.

```Swift
let date = Date()
let dateFormatter = DateFormatter()

dateFormatter.dateFormat = "yyyy년 MM월 dd일 HH시 mm분"
let dateString = dateFormatter.string(from: date)

print(dateString) // "2021년 10월 05일 12시 30분"
```

위 코드에서 ```Date()```는 현재 날짜와 시간을 나타내는 객체를 반환해줍니다. 그리고 이를 ```DateFormatter```의 ```string(from:)``` 메서드를 사용해 문자열로 변환합니다.

```DateFormatter``` 클래스의 ```dateFormat``` 속성을 이용하면 원하는 날짜 형식으로 문자열을 만들 수 있습니다. 자세한 날짜 형식에 대해서는 [공식 문서](https://developer.apple.com/documentation/foundation/dateformatter)를 참고해주세요.

## 깊이 파고들기

시간대와 로케일, 달력 등 다양한 환경 설정에 따라 날짜를 변환하는 방식이 달라질 수 있습니다. 이를 모두 고려해야 한다면 더 복잡한 작업이 될 수 있습니다. 따라서 날짜를 변환할 때는 가능한 정확한 방식을 적용하는 것이 중요합니다.

스위프트 뿐만 아니라 다른 프로그래밍 언어에서도 날짜를 문자열로 변환하는 기능을 제공합니다. 따라서 다른 언어에서도 일관된 방식으로 날짜를 변환할 수 있도록 학습하시는 것이 좋습니다.

## 더 알아보기

- [iOS 앱 개발: 날짜와 시간 처리하기](https://mokplus.github.io/62503009295/)
- [Swift Date and Time Tutorial: Formatting Dates](https://www.appcoda.com/date-time-tutorial-swift/)
- [ISO 8601 날짜 포맷](https://ko.wikipedia.org/wiki/ISO_8601)