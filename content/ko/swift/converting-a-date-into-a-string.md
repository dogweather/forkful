---
title:    "Swift: 날짜를 문자열로 변환하기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 이유는 데이터를 시각적으로 표현하기 쉽고 사람이 읽을 수 있는 형식으로 변환하기 위해서입니다. 예를 들어, 사용자가 생일을 입력할 때 숫자로 된 날짜보다는 "2010년 3월 17일"과 같이 문자열로 된 날짜를 입력하는 것이 더 쉽습니다.

## 하는 방법

먼저 `Date` 데이터 타입의 인스턴스를 만듭니다. 그 다음 `DateFormat`을 사용하여 원하는 날짜 형식을 지정합니다. 마지막으로 `String` 타입의 인스턴스를 만들고 `DateFormatter`를 사용하여 날짜를 문자열로 변환해줍니다. 아래는 예시 코드와 출력 결과입니다.

```Swift
let date = Date() // 현재 날짜
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
let dateString = formatter.string(from: date)
```

출력 결과: "2020-04-20"

## 깊이 파헤치기

`DateFormat`을 사용하여 날짜 형식을 지정할 때 사용할 수 있는 다양한 포맷 옵션이 있습니다. 예를 들어 `yyyy`는 연도를 4자리로 나타내고 `MM`은 월을 2자리로 나타냅니다. 또한 다양한 지역에서 사용하는 날짜 포맷을 지정해줄 수도 있습니다. 더 자세한 정보는 [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)을 참고해주세요.

## 또 다른 정보

- [Date to String with Swift](https://www.hackingwithswift.com/example-code/language/how-to-convert-a-swift-date-to-a-string-using-dateformatter)
- [Swift 날짜와 시간 포맷팅](https://zeddios.tistory.com/357)