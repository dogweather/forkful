---
title:                "Gleam: 미래나 과거에서의 날짜 계산"
simple_title:         "미래나 과거에서의 날짜 계산"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
미래 또는 과거의 날짜를 계산하는 것에 참여하는 이유는 정확한 시기를 알기 위해서입니다.

## 어떻게
"```Gleam ... ```" 코드 블록 안의 코딩 예제와 샘플 출력을 통해 설명합니다.

예제 1: 미래의 날짜 계산
```Gleam
import gleam/time/naive

let today = naive.DateTime.now()
let future_date = (today, { 1, Day })
```

예제 2: 과거의 날짜 계산
```Gleam
import gleam/time/naive

let today = naive.DateTime.now()
let past_date = (today, { -1, Year })
```

## 심층 분석
미래나 과거의 날짜를 계산하는 방법에는 몇 가지 다른 옵션이 있습니다. 예를 들어, 더 복잡한 계산을 위해 DateTime, TimeZone, TimeInterval 등의 모듈을 사용할 수도 있습니다. 또한 날짜를 특정 형식으로 포맷팅할 수도 있습니다. 이러한 기능들을 자세히 알아보고 싶다면 공식 Gleam 문서를 참조하시기 바랍니다.

## 관련 링크
- 온라인 Gleam 문서: https://gleam.run/documentation
- Gleam 날짜 및 시간 모듈: https://packages.gleam.run/search?q=time
- Gleam 언어 공식 레퍼런스: https://gleam.run/reference