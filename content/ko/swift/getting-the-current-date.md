---
title:                "현재 날짜 받아오기"
html_title:           "Swift: 현재 날짜 받아오기"
simple_title:         "현재 날짜 받아오기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜
지금 날짜를 얻는 것에 참여하는 이유는 무엇인가요?

현재 날짜를 얻는 것은 개발하는 데 매우 유용합니다. 예를 들어, 사용자에게 마지막으로 앱을 열람한 날짜를 보여주거나, 서버 프로그래밍에서 일련의 날짜를 생성하는 데 사용할 수 있습니다.

## 방법
지금 날짜를 얻는 방법은 간단합니다. "Date" 클래스의 인스턴스를 만들고 "Date()"로 호출하면 현재 날짜와 시간을 가져올 수 있습니다.

```Swift
let currentDate = Date() // 오늘의 날짜와 시간을 포함한 객체 생성
```

만약 포맷을 지정하여 현재 날짜를 보기 좋게 출력하고 싶다면 "DateFormatter" 클래스를 사용할 수 있습니다.

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "yyyy년 MM월 dd일"
let formattedDate = formatter.string(from: currentDate)

print(formattedDate) // 출력: 2021년 10월 23일
```

## 깊이 파고들기
"Date" 클래스는 "Foundation" 프레임워크에 속한 클래스로, 날짜와 시간을 다루는 데 유용한 메서드와 프로퍼티를 제공합니다.

또한 "Locale"을 사용하여 날짜와 시간을 표시하는 형식을 다르게 할 수도 있습니다. 예를 들어, 미국식으로 날짜를 표시하고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```Swift
let usLocale = Locale(identifier: "en_US")
let formatter = DateFormatter()
formatter.locale = usLocale
formatter.dateFormat = "MM/dd/yyyy"
let formattedDate = formatter.string(from: currentDate)

print(formattedDate) // 출력: 10/23/2021
```

## 더 알아보기
[Swift 공식 문서 - Date 클래스](https://developer.apple.com/documentation/foundation/date)
[날짜와 시간 다루기 관련 팁](https://www.developerinsider.in/things-to-consider-when-along-with-date-calendar-in-swift-5-using-latest-xcode-10/)
[날짜와 시간 다루는 다양한 예제 코드](https://www.hackingwithswift.com/example-code)