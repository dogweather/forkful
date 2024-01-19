---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

현재 날짜를 받아온다는 것은, 우리가 프로그래밍 중인 컴퓨터 시스템에서 현재의 날짜와 시간을 가져오는 것을 의미합니다. 이 기능은 로깅, 타임스탬프, 혹은 시간 정렬 등 다양한 코딩 태스크를 수행하기 위해 필요합니다.

## 어떻게 하는가:

Swift에는 현재 날짜와 시간을 얻는 방법이 내장되어 있습니다. 'Date()' 함수를 호출하기만 하면 됩니다:

```Swift
let 현재날짜 = Date()
print(현재날짜)
```

위 코드를 실행하면, 다음과 같은 출력이 나옵니다 (실행한 시간에 따라 출력은 달라집니다):
```
2022-02-10 12:00:00 +0000
```

## 심화 학습

Swift에 내장된 'Date()' 함수는 날짜와 시간을 단순하게 가져옵니다만, 사용하는 OS에 따라 해당 시스템 시간 기준으로 처리할 수 있습니다.

'Calendar'와 'DateComponents' 같은 클래스를 활용하면, 더 복잡하거나 세부적인 날짜 및 시간 정보를 가져올 수 있습니다. 예를 들어, 원하는 형식으로 날짜를 호출하거나, 현재 날짜를 기준으로 특정 시간을 계산하고 싶은 경우에 사용합니다.

```Swift
let calendar = Calendar.current
let dateComponents = calendar.dateComponents([.day, .month, .year, .hour, .minute, .second], from: Date())
```

위 코드는 현재 시스템에서 날짜와 시간을 가져와, 해당 날짜와 시간을 구성하는 요소들을 나누게 됩니다.

## 참고자료

더 많은 정보를 얻기 위해 다음 링크를 참조하세요:
- [Apple's 'Date' Documentation](https://developer.apple.com/documentation/foundation/date)
- [Apple's 'Calendar' Documentation](https://developer.apple.com/documentation/foundation/calendar)
- [Apple's 'DateComponents' Documentation](https://developer.apple.com/documentation/foundation/datecomponents)