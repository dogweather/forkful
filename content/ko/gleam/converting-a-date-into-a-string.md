---
title:                "Gleam: 날짜를 문자열로 변환하기"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것이 왜 중요한지 궁금하신가요? 날짜는 많은 데이터를 다루는 프로그래밍에서 중요한 요소입니다. 예를 들어 데이터베이스 쿼리를 작성하거나 시간 기반의 알림을 만들 때, 날짜를 적절한 형식으로 변환하는 것이 필수적입니다.

## 방법

다음은 Gleam에서 날짜를 문자열로 변환하는 예제입니다.

```Gleam
import gleam/time.String

let date = Time.Date.from_gregorian(2021,10,25)
let string = String.to_iso8601_date(date)

log(string) // 출력 결과: "2021-10-25"
```

위 코드는 먼저 `gleam/time` 모듈에서 `Date`를 불러와서 지정된 연도, 월, 일을 포함하는 날짜 객체를 생성합니다. 그리고 이를 `gleam/time/String` 모듈의 `to_iso8601_date` 함수에 전달하여 ISO 8601 표준 형식으로 변환합니다. 최종적으로 변환된 문자열을 출력합니다.

## 깊이있는 분석

날짜를 문자열로 변환하는 과정은 간단하지만 중요합니다. Gleam에서는 다양한 형식의 날짜를 지원하기 때문에, 예제에서는 ISO 8601 형식을 사용하였지만 다른 형식도 동일한 방법으로 변환할 수 있습니다. 또한 날짜의 포맷이나 타임존 등 옵션을 지정할 수 있어 원하는 결과를 얻을 수 있습니다. 더 자세한 정보는 [Gleam 공식 문서](https://gleam.run/documentation/)를 참고하시기 바랍니다.

## 또 다른 참고자료

- [Gleam에서 날짜 다루기](https://gleam.run/articles/dates/)
- [Gleam에서 문자열 다루기](https://gleam.run/articles/strings/)