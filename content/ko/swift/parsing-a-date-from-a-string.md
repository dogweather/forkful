---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:38:41.780116-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 날짜를 파싱한다는 것은, 문자의 나열을 날짜로 변경하는 과정입니다. 이것은 데이터를 사용자에게 친숙한 형식으로 보여주거나, 프로그램이 이해할 수 있는 방식으로 내부적으로 저장하려 할 때 필요합니다.

## How to: (실행 방법:)
```Swift
import Foundation

// 문자열로부터 Date 객체 생성
let dateString = "2023-04-05T14:10:00+0000"
let dateFormatter = ISO8601DateFormatter()
if let date = dateFormatter.date(from: dateString) {
    print(date)
} else {
    print("날짜 변환에 실패했습니다.")
}

// 출력 예시
// 2023-04-05 14:10:00 +0000
```

## Deep Dive (심층 탐구)
문자열에서 날짜 파싱은 1970년대부터 있었습니다. 먼저, 간단한 형식 문자열에서 시작해, 나중에는 ISO8601과 같은 복잡한 표준 형식으로 발전했습니다. ISO8601 형식은 국제 표준으로, 여러 시간대와 날짜-시간 형식을 지원합니다.

Swift에서는 `DateFormatter`와 `ISO8601DateFormatter` 같은 내장 클래스를 사용하여 이 작업을 수행합니다. `DateFormatter`는 커스텀 패턴을 사용해 더 복잡한 날짜 형식에도 대응할 수 있고, `ISO8601DateFormatter`는 표준 ISO8601 형식에 최적화되어 있습니다.

```Swift
// 사용자 정의 날짜 형식
let customDateString = "05-04-2023 14:10"
let customFormatter = DateFormatter()
customFormatter.dateFormat = "dd-MM-yyyy HH:mm"
if let customDate = customFormatter.date(from: customDateString) {
    print(customDate)
} else {
    print("날짜 변환에 실패했습니다.")
}
```

## See Also (참고 자료)
- [Date Formatting Guide](https://developer.apple.com/documentation/foundation/dateformatter)
- [ISO8601DateFormatter](https://developer.apple.com/documentation/foundation/iso8601dateformatter)
- Swift 공식 문서 내의 날짜 및 시간 조작에 대한 자세한 내용: [Working with Date and Time](https://developer.apple.com/documentation/foundation/dates_and_times)
