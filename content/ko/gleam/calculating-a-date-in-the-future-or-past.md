---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Gleam: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜 계산하는가

우리는 많은 일들을 할 때 날짜를 계산해야 할 때가 많습니다. 예를 들어, 모임을 잡을 때, 여행을 계획할 때, 혹은 어떤 일의 완료 날짜를 설정할 때 등등. 만약 쉽게 날짜를 계산할 수 있다면, 우리는 더 많은 시간을 활용할 수 있고, 더 많은 일을 할 수 있습니다.

## 계산하는 방법

우리는 Gleam을 사용하여 간단하게 날짜를 계산할 수 있습니다. 먼저, `calendar` 라이브러리를 가져오고, 날짜와 계산할 시간을 입력합니다. 그 후 `add` 함수를 사용하여 입력한 시간을 더하거나 빼줍니다. 예를 들어, 만약 내일로부터 한 달 후의 날짜를 계산하고 싶다면 다음과 같이 입력할 수 있습니다.

```Gleam
import calendar

future_date = add(calendar.today(), 1, "months")
```

위의 코드는 `calendar.today()`를 사용하여 오늘의 날짜를 가져온 후, 그 날짜에 1개월을 더한 결과를 `future_date` 변수에 할당합니다. 이제 우리는 `future_date` 변수에 원하는 날짜를 계산하여 사용할 수 있습니다.

## 심화 계산

날짜 계산은 간단해 보이지만, 실제로는 깊게 파고들어야 하는 작업입니다. 우리는 날짜와 시간을 다루는 많은 변수를 고려해야 합니다. 예를 들어, 모든 해는 윤년이 아니기 때문에 윤년 계산을 따로 해줘야 합니다. 또한 특정 날짜가 범위를 벗어나는 경우도 처리해줘야 합니다.

더 심화된 날짜 계산을 위해서는 Gleam의 `calendar` 라이브러리를 더 빠르고 효율적으로 사용하는 방법을 공부해보세요!

## 그 외 참고 자료

- [Gleam 공식 홈페이지](https://gleam.run/)
- [Gleam GitHub 레포지토리](https://github.com/gleam-lang/gleam)
- [Gleam 라이브러리 공식 문서](https://gleam.run/documentation/)
- [Gleam 커뮤니티 Slack 채널](https://gleam-lang.slack.com/)