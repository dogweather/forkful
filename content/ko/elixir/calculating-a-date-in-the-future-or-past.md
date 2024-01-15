---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Elixir: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

우리는 모두 일정을 관리하고 계획하느라 바쁜 삶을 살고 있습니다. 때로는 미래나 과거의 날짜를 계산하는 것이 필요할 때가 있죠. 이러한 상황에서 우리는 Elixir를 사용하여 손쉽게 다가오는 또는 지난 날짜를 계산할 수 있습니다. 

## 할 수 있는 방법

아래는 `future_date`와 `past_date` 함수를 사용하여 미래와 과거의 날짜를 계산하는 예제입니다. 

```elixir
future_date = Date.add(Date.utc_today(), 10)
past_date = Date.add(Date.utc_today(), -10)

IO.inspect future_date #=> ~D[2021-07-05]
IO.inspect past_date #=> ~D[2021-06-15]
```

위의 예제에서 `Date.add` 함수는 현재 날짜에 대한 새로운 날짜를 계산합니다. 첫 번째 인자는 기준이 되는 날짜를, 두 번째 인자는 해당 날짜에 더하거나 뺄 날짜를 나타냅니다. 따라서 위의 예제에서는 현재 날짜로부터 10일 후의 날짜와 10일 전의 날짜를 계산하였습니다. 

## 딥 다이브

Elixir의 `Date.add` 함수는 `Calendar` 모듈에서 제공하며, 날짜 및 시간 연산을 포함하는 다양한 함수들이 있습니다. 이를 통해 우리는 더 복잡한 날짜 계산을 수행할 수 있습니다. 예를 들어, `add` 함수 대신 `diff` 함수를 사용하여 두 날짜 간의 차이를 계산할 수 있습니다. 

또한, Elixir의 `DateTime` 타입은 초단위까지 정밀한 시간을 처리할 수 있으며, `Timex` 라이브러리를 사용하면 더 다양한 시간 관련 기능을 제공받을 수 있습니다.

## 알아보기

- [Elixir Date 모듈 참조](https://hexdocs.pm/elixir/Date.html)
- [Elixir Calendar 모듈 참조](https://hexdocs.pm/elixir/Calendar.html)
- [Timex 라이브러리](https://hexdocs.pm/timex/readme.html)

## 참고하기

- [Date and Time calculations in Elixir](https://medium.com/swlh/date-and-time-calculations-in-elixir-3bfcda1d9f72)
- [Working with Dates and Time in Elixir using Timex](https://medium.com/@baldwindavid/working-with-dates-and-times-in-elixir-using-timex-6fbcbaa3347b)