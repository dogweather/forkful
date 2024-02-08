---
title:                "현재 날짜 가져오기"
date:                  2024-02-03T19:11:13.658538-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Swift에서 현재 날짜를 가져오는 것은 `Date` 클래스를 사용하여 앱이 실행되는 날짜와 시간에 접근하는 것을 포함합니다. 프로그래머들은 이벤트의 타임스탬프 적용, 날짜 계산 수행, 사용자 인터페이스에 날짜와 시간 표시 등 다양한 이유로 현재 날짜를 가져와야 합니다.

## 어떻게:
Swift의 `Foundation` 프레임워크는 `Date` 클래스를 제공하여 현재 날짜와 시간을 쉽게 가져올 수 있습니다. 현재 날짜를 가져오는 기본 예는 다음과 같습니다:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

이것은 다음과 같은 것을 출력합니다:

```
2023-04-12 07:46:23 +0000
```

출력 형식은 UTC 시간대를 사용하는 ISO 8601 표준을 따릅니다. 하지만, 이 날짜를 표시 목적으로 형식을 지정하고 싶을 수 있습니다. Swift의 `DateFormatter` 클래스가 이때 도움을 줍니다:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

샘플 출력은 다음과 같을 수 있습니다:

```
2023년 4월 12일 오전 10시 46분 23초
```

출력 형식은 코드를 실행하는 디바이스의 로케일에 따라 다를 수 있습니다.

더 복잡한 날짜 조작이 필요한 프로젝트의 경우, 많은 Swift 개발자들이 `SwiftDate` 같은 타사 라이브러리를 사용합니다. 특정 시간대 및 형식으로 현재 날짜를 가져오는 방법은 다음과 같습니다:

먼저 SPM, CocoaPods, 또는 Carthage를 사용하여 프로젝트에 `SwiftDate`를 추가합니다. 그 후:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

이것은 다음과 같이 출력할 수 있습니다:

```
2023-04-12 09:46:23
```

`SwiftDate`를 사용하면 다양한 시간대 및 로케일에 대한 날짜와 시간을 쉽게 조작하여 Swift 애플리케이션에서 복잡한 날짜 처리 작업을 단순화할 수 있습니다.
