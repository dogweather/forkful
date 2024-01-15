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

## 왜

날짜 비교를 하는 이유는 어떤 날짜가 더 늦은지, 더 이른지 혹은 두 날짜가 같은지 비교하기 위해서입니다.

## 어떻게

```Gleam
import gleam/time

let date1 = time.new(2021, 3, 15)
let date2 = time.new(2021, 2, 2)

let comparison = time.compare(date1, date2)

// comparison = time.CompareTo.(after | before | equal)
```

위의 코드를 보면 Gleam의 시간 라이브러리를 사용해 두 날짜를 비교하는 방법을 보여줍니다. `time.compare` 함수를 이용해 두 날짜를 비교하고 결과는 `time.CompareTo` 타입으로 반환됩니다. 이를 이용해 어떤 날짜가 이후인지, 이전인지 혹은 같은 날짜인지 확인할 수 있습니다.

## 깊게 파헤치기

이 외에도 Gleam의 시간 라이브러리에는 날짜 간 간격을 계산하는 `time.diff` 함수, 기간을 더하거나 뺄 수 있는 `time.add`와 `time.subtract` 함수, 그리고 주어진 날짜의 요일을 반환하는 `time.weekday` 함수 등 다양한 기능이 있습니다. 적절하게 조합하면 다양한 날짜와 시간 계산을 쉽고 빠르게 처리할 수 있습니다.

## 더 알아보기

- [Gleam 공식 홈페이지](https://gleam.run/)
- [Gleam의 시간 라이브러리 문서](https://gleam.run/modules/time)