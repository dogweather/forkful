---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:16:39.022321-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜와 시간을 받아오는 것은 현재의 날짜와 시간을 알아내는 과정입니다. 일자 기록, 타이머, 데드라인 설정 등 다양한 기능에서 필수적입니다.

## How to: (어떻게 하나요?)
```Swift
import Foundation

let now = Date()
print(now)
```

```Swift
// 샘플 출력
2023-04-05 12:00:00 +0000
```

## Deep Dive (심층 탐구)
스위프트에서 날짜와 시간은 `Date` 구조체를 사용해 표현됩니다. 'Foundation' 프레임워크에 포함되어 있죠. UTC(협정 세계시) 기준으로 표시합니다. 타임존을 적용하려면 `DateFormatter`나 `Calendar`를 사용해야 합니다. 이전에는 Objective-C의 `NSDate`를 사용했지만 Swift가 출시되면서 더 간편하고 안전한 `Date`로 전환되었어요. 'DateComponents', 'TimeInterval', 'Timezone' 등과 함께 사용하여 다양한 날짜 수학과 조작 기능을 수행할 수 있습니다.

## See Also (더 보기)
- Swift의 공식 `Date` 문서: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- DateFormatter 사용법: [https://developer.apple.com/documentation/foundation/dateformatter](https://developer.apple.com/documentation/foundation/dateformatter)
- 시간대와 캘린더 사용법: [https://developer.apple.com/documentation/foundation/timezone](https://developer.apple.com/documentation/foundation/timezone) 및 [https://developer.apple.com/documentation/foundation/calendar](https://developer.apple.com/documentation/foundation/calendar)
