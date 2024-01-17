---
title:                "두 날짜 비교하기"
html_title:           "Gleam: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

자 날짜 비교를 해볼까?

날짜 비교는 무엇인가?

날짜 비교는 두 개의 날짜를 비교하여 어떤 것이 더 큰지 혹은 작은지를 판단하는 것입니다. 프로그래머들은 주로 날짜 관련 문제를 다룰 때, 예를 들어 만료일 확인이나 이벤트 일자를 비교하는 경우 등등에 많이 사용합니다.

어떻게 해야 할까?

Gleam에서 날짜를 비교하기 위해서는 ```Gleam Date``` 타입 내장 함수들을 이용하면 됩니다.

```Gleam
let date_1 = Date.new(2021, 7, 12)
let date_2 = Date.new(2021, 7, 21)

Date.compare(date_1, date_2) // -1 출력
Date.compare(date_2, date_1) // 1 출력
Date.compare(date_1, date_1) // 0 출력
```

깊이 파보자

날짜 비교는 컴퓨터 프로그래밍의 역사와 밀접하게 연관되어 있습니다. 오래된 컴퓨터 체계에서는 년, 월, 일을 비트로 나타내야 했기 때문에 날짜 비교는 복잡한 알고리즘을 필요로 했지만, 현재는 문자열로 표현할 수 있는 표준 포맷인 ISO-8601을 사용하여 더 쉽게 날짜를 처리할 수 있게 되었습니다.

날짜 비교에는 여러 가지 방법이 있습니다. 대표적인 방식으로는 첫 번째 날짜와 두 번째 날짜를 빼서 양수, 음수, 0으로 비교하는 방식과, 각 날짜를 일수로 변환하여 비교하는 방식이 있습니다. 하지만 Gleam에서는 간편하게 Date 타입 내장 함수인 ```compare```를 사용하여 비교할 수 있습니다.

관련 자료 확인하기

- [Gleam 공식 문서](https://gleam.run/documentation/date/)
- [ISO-8601 날짜 표준 포맷](https://www.iso.org/iso-8601-date-and-time-format.html)
- [코드 위젯 라이브러리 moment.js](https://momentjs.com/)