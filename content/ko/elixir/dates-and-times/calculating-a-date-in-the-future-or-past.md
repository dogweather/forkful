---
date: 2024-01-20 17:30:48.212170-07:00
description: "\uBBF8\uB798 \uD639\uC740 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\
  \uC740 \uD2B9\uC815 \uAE30\uC900\uC77C\uB85C\uBD80\uD130 \uC77C\uC815 \uAE30\uAC04\
  \ \uD6C4\uB098 \uC804\uC758 \uB0A0\uC9DC\uB97C \uCC3E\uB294 \uAC83\uC785\uB2C8\uB2E4\
  . \uC774\uB294 \uB9CC\uAE30\uC77C, \uC774\uBCA4\uD2B8 \uC2A4\uCF00\uC904\uB9C1,\
  \ \uAE30\uAC04 \uBC94\uC704 \uD655\uC778 \uB4F1\uC758 \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uC0C1\uD669\uC5D0\uC11C \uD544\uC694\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.741381-06:00'
model: gpt-4-1106-preview
summary: "\uBBF8\uB798 \uD639\uC740 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uC740\
  \ \uD2B9\uC815 \uAE30\uC900\uC77C\uB85C\uBD80\uD130 \uC77C\uC815 \uAE30\uAC04 \uD6C4\
  \uB098 \uC804\uC758 \uB0A0\uC9DC\uB97C \uCC3E\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

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
