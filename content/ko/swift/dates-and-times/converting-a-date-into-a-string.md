---
title:                "날짜를 문자열로 변환하기"
aliases:
- /ko/swift/converting-a-date-into-a-string/
date:                  2024-01-20T17:37:32.325879-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환하는 것은 날짜 데이터를 쉽게 읽을 수 있는 텍스트 형태로 바꾸는 작업입니다. 프로그래머들은 사용자 인터페이스에 표시하거나, 데이터를 로깅 및 서버 전송을 위해 이런 변환을 합니다.

## How to: (방법)
Swift에서 날짜를 문자열로 변환하려면 `DateFormatter`를 사용합니다. 아래의 예시를 참고하세요.

```Swift
import Foundation

// 날짜 생성하기
let now = Date()

// DateFormatter 인스턴스 생성
let dateFormatter = DateFormatter()

// 변환 형식 설정하기
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"

// Date -> String 변환
let dateString = dateFormatter.string(from: now)

// 출력
print(dateString) // "2023-04-05 14:23:36" 예시 출력
```

`dateFormat`은 날짜와 시간의 형식을 결정합니다. 적절한 형식 문자열을 사용하여 원하는 출력을 얻을 수 있습니다.

## Deep Dive (심층 분석)
날짜 형식 변환은 프로그래밍 초기부터 필요한 기능이었습니다. 표준화된 날짜 및 시간 형식이 필요한 사용 사례가 늘면서, 여러 프로그래밍 언어는 이를 위한 도구를 갖추게 되었습니다.

Swift는 `DateFormatter`를 제공하여 날짜와 문자열 간의 변환을 가능하게 합니다. `dateFormat`에서 사용하는 형식 문자열은 Unicode 기술 표준인 'Date Format Patterns'을 기반으로 합니다.

`DateFormatter` 이외에도 `ISO8601DateFormatter`가 있으며, 특히 ISO 8601 날짜 형식 변환에 최적화되어 있습니다. JSON과 같은 데이터 형식과 일할 때 유용하죠.

```Swift
import Foundation

// ISO8601DateFormatter 인스턴스 생성
let isoFormatter = ISO8601DateFormatter()

// Date -> String 변환
let isoDateString = isoFormatter.string(from: now)

// 출력
print(isoDateString) // "2023-04-05T14:23:36Z" 예시 출력
```

또한, Swift의 `Date`와 `String` 변환은 시스템의 지역 설정이나 시간대 영향을 받을 수 있습니다. 이런 변수를 적절히 관리하는 것은 국제화된 앱에서 중요합니다.

## See Also (참고하기)
- Apple Date Formatting Guide: [https://developer.apple.com/documentation/foundation/dateformatter](https://developer.apple.com/documentation/foundation/dateformatter)
- Unicode Date Format Patterns: [http://unicode.org/reports/tr35/tr35-25.html#Date_Format_Patterns](http://unicode.org/reports/tr35/tr35-25.html#Date_Format_Patterns)
- Swift `Date` and `Calendar` Documentation: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
