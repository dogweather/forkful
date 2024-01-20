---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Elixir: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 뭐고 왜?

날짜 계산은 미래나 과거의 특정 날짜를 산출하는 것을 의미합니다. 이는 주기적인 작업 스케줄링, 이벤트 기록, 한 해의 경과 일 등을 계산할 때 프로그래머들이 사용합니다.

## 이렇게 해보세요:

```Elixir
# 미래의 날짜를 계산하는 방법:
{:ok, future_date} = Date.add(Date.utc_today(), 10)
IO.inspect(future_date)

# 과거의 날짜 계산:
{:ok, past_date} = Date.add(Date.utc_today(), -5)
IO.inspect(past_date)
```
Sample Output:
```Elixir
~D[2023-12-13]
~D[2023-11-28]
```
## 깊게 들어가보기:

날짜 계산은 컴퓨터 프로그래밍의 중요한 요소 중 하나입니다. 특히, 날짜 및 시간에 기반한 소프트웨어에서는 이것이 굉장히 중요합니다. Elixir 등의 프로그래밍 언어들은 이러한 작업을 위해 내장 함수를 제공하므로 사용자가 본인이 원하는 날짜를 쉽게 계산할 수 있을 것입니다.

잘 알려진 대안으로는 Python의 datetime 라이브러리와 같은 외부 라이브러리들을 사용하는 것이 있습니다. 또한, Elixir에서 날짜 계산을 구현하는 방식은 Erlang의 calendar 모듈을 기반으로 하며, 이 모듈은 사용자가 직접 날짜 계산을 수행할 수 있도록 함께 제공됩니다.

## 참고 자료:

- Official Elixir Date documentation: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
- Elixir programming examples: [https://github.com/elixir-lang/elixir](https://github.com/elixir-lang/elixir)
- Erlang's calendar module: [http://erlang.org/doc/man/calendar.html](http://erlang.org/doc/man/calendar.html) 
- Python's datetime library for comparison: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)