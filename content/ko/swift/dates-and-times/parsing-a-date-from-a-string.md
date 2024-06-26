---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:45.926705-07:00
description: "\uC5B4\uB5BB\uAC8C: Swift\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uC778 Foundation\uC740 \uBB38\uC790\uC5F4\uC744 `Date` \uAC1D\uCCB4\uB85C\
  , \uADF8\uB9AC\uACE0 \uADF8 \uBC18\uB300\uB85C \uBCC0\uD658\uD558\uAE30 \uC704\uD55C\
  \ `DateFormatter`\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4. \uBB38\uC790\uC5F4\uC5D0\
  \uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uAE30 \uC704\uD574, \uBB38\uC790\uC5F4\
  \uACFC \uC77C\uCE58\uD558\uB294 \uB0A0\uC9DC \uD615\uC2DD\uC744 \uC9C0\uC815\uD55C\
  \ \uB2E4\uC74C \uD3EC\uB9F7\uD130\uB97C \uC0AC\uC6A9\uD558\uC5EC \uD574\uC11D\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.744967-06:00'
model: gpt-4-0125-preview
summary: "Swift\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC778 Foundation\uC740\
  \ \uBB38\uC790\uC5F4\uC744 `Date` \uAC1D\uCCB4\uB85C, \uADF8\uB9AC\uACE0 \uADF8\
  \ \uBC18\uB300\uB85C \uBCC0\uD658\uD558\uAE30 \uC704\uD55C `DateFormatter`\uB97C\
  \ \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 어떻게:


### Foundation의 `DateFormatter` 사용하기
Swift의 표준 라이브러리인 Foundation은 문자열을 `Date` 객체로, 그리고 그 반대로 변환하기 위한 `DateFormatter`를 제공합니다. 문자열에서 날짜를 파싱하기 위해, 문자열과 일치하는 날짜 형식을 지정한 다음 포맷터를 사용하여 해석합니다.

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("파싱된 날짜: \(date)")
} else {
    print("날짜 파싱 실패")
}
// 샘플 출력: 파싱된 날짜: 2023-04-29 22:00:00 +0000
```

출력은 사용자의 시간대에 따라 달라질 수 있습니다.

### ISO8601DateFormatter 사용하기
ISO 8601 날짜 형식에 대해, Swift는 파싱 과정을 간소화하는 전문 포맷터인 `ISO8601DateFormatter`를 제공합니다.

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("파싱된 ISO8601 날짜: \(date)")
} else {
    print("ISO8601 날짜 파싱 실패")
}
// 샘플 출력: 파싱된 ISO8601 날짜: 2023-04-30 15:00:00 +0000
```

### 서드파티 라이브러리 사용하기: SwiftDate
Swift는 강력한 날짜 파싱 도구를 제공하지만, SwiftDate 같은 서드파티 라이브러리는 더 큰 유연성과 편리함을 제공합니다. 프로젝트에 SwiftDate를 추가한 후에는 파싱이 아주 간단해집니다:

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("SwiftDate로 파싱된 날짜: \(date)")
} else {
    print("SwiftDate로 날짜 파싱 실패")
}
// 샘플 출력: SwiftDate로 파싱된 날짜: 2023-04-30 00:00:00 +0000
```

SwiftDate는 자연어와 다양한 날짜 형식으로 파싱을 단순화하여, Swift 프로그래밍 도구 상자에 강력한 추가 요소로 자리 잡습니다.
