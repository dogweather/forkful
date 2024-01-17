---
title:                "날짜를 문자열로 변환하기"
html_title:           "Gleam: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은 우리가 일상적으로 하는 작업 중 하나입니다. 예를 들어, 생일 카드를 작성하거나 약속 시간을 기록할 때, 날짜를 문자열로 만들어서 다른 사람들과 공유합니다. 프로그래머들은 이 작업을 컴퓨터가 날짜를 이해할 수 있는 형식으로 변환하기 위해 수행합니다.

## 하는 방법:

```Gleam
let date = Time.now()
let string = Time.to_string(date, "%Y-%m-%d")
```

위 코드를 실행하면, 오늘 날짜를 `2021-08-04`와 같은 형식으로 출력할 수 있습니다.

```Gleam
let long_string = Time.to_string(date, "%A, %B %d, %Y")
```

이렇게 실행하면 오늘 날짜를 `Wednesday, August 04, 2021`과 같은 긴 형식으로 출력할 수 있습니다.

## 깊게 파고들기:

날짜를 문자열로 변환하는 방법은 시간의 흐름과 컴퓨터 과학의 발전과 무관하지는 않습니다. 예를 들어, 1582년은 그레고리력이 도입된 첫 해인데, 이 때부터 현재 사용되는 날짜 형식이 정립되었습니다. 또한, 사람들은 오랫동안 날짜와 시간을 컴퓨터가 이해할 수 있는 숫자로 표현하려고 노력해 왔는데, 이를 위해 다양한 날짜 형식과 알고리즘이 개발되었습니다.

날짜를 문자열로 변환하는데 사용할 수 있는 대안으로는 날짜를 데이터베이스에 저장하거나, 인간이 이해하기 쉬운 날짜 형식으로 변환하는 것이 있습니다. 그러나, 컴퓨터가 날짜를 이해하고 연산하기에 가장 효율적인 방법은 숫자 형식으로 변환하는 것입니다.

## 더 알아보기:

- [Gleam 공식 문서](https://gleam.run/documentation/standard_library#time-date-to_string)에서 `Time.to_string`의 자세한 정보를 확인할 수 있습니다.
- [ISO 8601](https://ko.wikipedia.org/wiki/ISO_8601)는 날짜와 시간에 대한 국제 표준을 정의한 문서입니다.
- [Date & Time formats in programming](https://www.codecademy.com/articles/fwd-js-date-and-time-formats)에서 날짜와 시간 형식에 대한 더 많은 정보를 얻을 수 있습니다.