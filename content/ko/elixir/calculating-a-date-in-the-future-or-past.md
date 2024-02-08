---
title:                "미래나 과거의 날짜 계산하기"
aliases:
- ko/elixir/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:30:48.212170-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
미래 혹은 과거의 날짜 계산은 특정 기준일로부터 일정 기간 후나 전의 날짜를 찾는 것입니다. 이는 만기일, 이벤트 스케줄링, 기간 범위 확인 등의 프로그래밍 상황에서 필요합니다. 

## How to:
Elixir에서는 `Timex` 라이브러리를 사용해 날짜 계산을 쉽게 할 수 있습니다. 

```elixir
# Timex 설치가 필요합니다: {:timex, "~> 3.7"}
defmodule DateCalculator do
  import Timex

  def add_days_to_date(date, days) do
    date |> Timex.add(days: days)
  end

  def subtract_days_from_date(date, days) do
    date |> Timex.subtract(days: days)
  end
end

# 오늘로부터 10일 후
IO.inspect DateCalculator.add_days_to_date(Timex.today(), 10)

# 오늘로부터 10일 전
IO.inspect DateCalculator.subtract_days_from_date(Timex.today(), 10)
```

예상 출력:
```
~N[2023-04-24 00:00:00]
~N[2023-04-04 00:00:00]
```

## Deep Dive
그레고리력이 도입되기 이전에는 율리우스력을 사용했고, 날짜 계산이 더 복잡했습니다. 시스템에 따라서도 날짜 처리가 달랐습니다.

Timex는 Elixir에서 날짜와 시간을 다루기 위해 널리 사용되는 라이브러리입니다. 내부적으로는 Erlang의 `:calendar`와 `:datetime` 모듈을 활용합니다. 

Elixir 표준 라이브러리에서도 `Date`, `DateTime` 모듈을 통해 기본적인 날짜 계산 기능을 제공하지만, Timex는 시간대 관리, 포맷팅, 파싱 등 추가 기능을 포함하고 있어 더 강력합니다.

다른 언어에서는 다른 라이브러리를 사용할 수도 있습니다. 예를 들면, Python에서는 `datetime` 모듈, JavaScript에서는 `Date` 객체 또는  `moment.js` 라이브러리 등이 있습니다.

## See Also
- Timex 공식 문서: [https://hexdocs.pm/timex/Timex.html](https://hexdocs.pm/timex/Timex.html)
- Elixir `Date` 모듈 공식 문서: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
- Erlang `:calendar` 모듈 공식 문서: [http://erlang.org/doc/man/calendar.html](http://erlang.org/doc/man/calendar.html)
