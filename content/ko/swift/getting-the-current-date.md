---
title:                "Swift: 현재 날짜 가져 오기"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오는 것에 대해 궁금해 하는 이유는 매우 자연스러운 일입니다. 많은 앱과 웹 사이트들이 일반적으로 현재 날짜를 화면에 표시하기 때문입니다. Swift를 배우고 있다면, 현재 날짜를 코드에서 가져오는 방법을 배우는 것은 중요합니다.

## 어떻게

Swift로 현재 날짜를 가져오는 방법은 간단합니다. Date() 함수를 사용하면 현재 날짜와 시간을 나타내는 인스턴스를 만들 수 있습니다. 또한 DateFormatter 객체를 사용하여 현재 날짜를 원하는 형식으로 변환할 수 있습니다.

```Swift
// 현재 날짜와 시간을 나타내는 Date 객체 생성
let currentDate = Date()

// DateFormatter 객체 생성
let formatter = DateFormatter()

// 원하는 형식으로 날짜 포맷 지정
formatter.dateFormat = "yyyy-MM-dd"

// 날짜를 지정한 형식으로 변환
let formattedDate = formatter.string(from: currentDate)

// 콘솔 출력: 2021-06-15
print(formattedDate)
```

또는 원하는 형식이 없다면, 간단하게 문자열 인터폴레이션을 사용하여 현재 날짜를 표시할 수도 있습니다.

```Swift
// 현재 날짜를 그대로 문자열로 출력
let currentDate = "\(Date())"

// 콘솔 출력: 2021-06-15 07:00:00 +0000
print(currentDate)
```

## 딥 다이브

Swift에서는 현재 날짜를 가져오는 다양한 방법이 있습니다. Date() 함수 외에도 DateComponents 객체를 사용하여 현재 날짜의 원하는 정보(년, 월, 일, 시간, 분, 초 등)를 추출할 수 있습니다. 또한, TimeZone 객체를 사용하여 특정 지역의 현재 날짜를 알아낼 수도 있습니다. 더 많은 정보를 알고 싶다면, 공식 문서를 참고해보세요.

## 참고

[Swift 공식 문서](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID322) 
[Swift Date 포맷 가이드](https://developer.apple.com/documentation/foundation/dateformatter)
[Swift TimeZone 클래스](https://developer.apple.com/documentation/foundation/timezone)