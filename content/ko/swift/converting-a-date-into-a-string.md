---
title:                "Swift: 날짜를 문자열로 변환하기"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것에 관심이 있을 수도 있습니다. 날짜와 관련된 정보를 저장하고 표시하기 위해서라던지, 날짜에 따라 다른 작업을 수행하기 위해서일 수도 있습니다.

## 어떻게

날짜를 문자열로 변환하는 방법을 살펴보겠습니다. Swift에서는 `DateFormatter`를 사용하여 날짜를 원하는 형식의 문자열로 변환할 수 있습니다. 아래 코드는 `DateFormatter`를 사용하여 현재 날짜를 YYYY/MM/DD 형식의 문자열로 변환하는 예시입니다.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "YYYY/MM/DD"
let currentDate = Date()
let stringDate = dateFormatter.string(from: currentDate)
print(stringDate) // 2021/05/19
```

위의 코드를 실행하면 현재 날짜가 2021년 5월 19일이라면 `"2021/05/19"`라는 형식의 문자열이 출력될 것입니다. `DateFormatter`의 `dateFormat` 속성을 원하는 형식으로 지정해주면 해당 형식으로 날짜를 문자열로 변환할 수 있습니다.

## 깊게 파고들기

`DateFormatter`를 사용하여 날짜를 문자열로 변환하는 것은 간단하지만, 더 많은 옵션들을 살펴보고 싶을 수도 있습니다. 예를 들어 년, 월, 일 등 다양한 단위로 날짜를 표시하거나 지역 설정에 따라 다른 언어로 출력하는 방법도 알아볼 수 있습니다.

## 관련 링크

- [Swift 공식 문서: Formatter Class Reference](https://developer.apple.com/documentation/foundation/formatter)
- [날짜 및 시간 변환기를 사용하여 문자열을 날짜로 변환하는 방법 (Swift)](https://stackoverflow.com/questions/35700281/how-to-convert-string-to-date-using-date-formatter-in-swift/35700828)
- [Swift에서 날짜 다루기](https://www.hackingwithswift.com/articles/117/swift-5-4-brings-a-new-date-formatting-api)

## 참고

- [마크다운 (Markdown) 살펴보기](https://gist.github.com/ihoneymon/652be052a0727ad59601)
- [Kakao Markdown 사용법](https://story.kakao.com/ch/markdown)