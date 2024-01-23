---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:14:06.089842-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜를 얻는 것은 시스템의 현재 날짜와 시간을 알아내는 과정입니다. 프로그래머들은 로그 기록, 날짜별 데이터 처리, 사용자 경험 개선 등을 위해 이를 사용합니다.

## How to: (어떻게 하는가)
Elixir에서 현재 날짜를 얻으려면 `Date` 모듈을 사용합니다. 아래 예시 코드와 출력된 결과를 확인해보세요.

```elixir
# DateTime 모듈을 사용하여 현재 시각을 얻기
current_datetime = DateTime.utc_now()
IO.inspect(current_datetime)

# Date 모듈만 사용하여 현재 날짜 정보를 얻기
current_date = Date.utc_today()
IO.inspect(current_date)
```

예상 출력:
```
# DateTime.utc_now의 결과
~U[2023-04-05 12:34:56Z]

# Date.utc_today의 결과
~D[2023-04-05]
```

## Deep Dive (심층 분석)
Elixir에서는 `DateTime`과 `Date` 모듈로 날짜와 시간을 관리합니다. 이 기능은 내부적으로 Erlang의 시간 관리 기능을 활용합니다. `DateTime.utc_now/0`는 UTC 시간대기준의 현재 시각을, `Date.utc_today/0`는 UTC 기준의 오늘 날짜를 반환합니다.

과거 Erlang에서는 직접 시간을 계산하거나 외부 라이브러리를 사용해야 했지만, Elixir의 `DateTime`과 `Date`는 이러한 복잡성을 추상화하고 더 쉬운 날짜 관리를 가능하게 합니다.

## See Also (추가 자료)
- Elixir 공식 문서의 `DateTime` 모듈: [DateTime](https://hexdocs.pm/elixir/DateTime.html)
- Elixir 공식 문서의 `Date` 모듈: [Date](https://hexdocs.pm/elixir/Date.html)
- Erlang 문서의 `calendar` 모듈, 기초적인 시간 계산을 위한 함수를 제공: [Erlang calendar](http://erlang.org/doc/man/calendar.html)
