---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:13.658538-07:00
description: "Swift\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\
  \uB294 \uAC83\uC740 `Date` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC571\
  \uC774 \uC2E4\uD589\uB418\uB294 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC5D0 \uC811\uADFC\
  \uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC774\uBCA4\uD2B8\uC758 \uD0C0\uC784\uC2A4\uD0EC\uD504 \uC801\
  \uC6A9, \uB0A0\uC9DC \uACC4\uC0B0 \uC218\uD589, \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\
  \uC774\uC2A4\uC5D0 \uB0A0\uC9DC\uC640 \uC2DC\uAC04 \uD45C\uC2DC \uB4F1 \uB2E4\uC591\
  \uD55C \uC774\uC720\uB85C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC640\uC57C\
  \ \uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.727629-07:00'
model: gpt-4-0125-preview
summary: "Swift\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294\
  \ \uAC83\uC740 `Date` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC571\uC774\
  \ \uC2E4\uD589\uB418\uB294 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC5D0 \uC811\uADFC\uD558\
  \uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC774\uBCA4\uD2B8\uC758 \uD0C0\uC784\uC2A4\uD0EC\uD504 \uC801\uC6A9\
  , \uB0A0\uC9DC \uACC4\uC0B0 \uC218\uD589, \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\uC774\
  \uC2A4\uC5D0 \uB0A0\uC9DC\uC640 \uC2DC\uAC04 \uD45C\uC2DC \uB4F1 \uB2E4\uC591\uD55C\
  \ \uC774\uC720\uB85C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC640\uC57C \uD569\
  \uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
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
