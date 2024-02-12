---
title:                "날짜를 문자열로 변환하기"
aliases:
- /ko/elixir/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:09.893910-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가? 그리고 왜?)
날짜를 문자열로 변환하는 것은 일정한 형식의 문자열로 날짜를 표현하는 작업입니다. 이는 로깅, 사용자 인터페이스 표시 또는 다른 시스템과의 날짜 데이터 교환을 위해 주로 사용됩니다.

## How to: (방법:)
```elixir
# Elixir에서 현재 날짜를 문자열로 변환합니다.
date = Date.utc_today()

# ISO8601 형식으로 변경
date_iso_string = Date.to_iso8601(date)
IO.puts date_iso_string  # "2023-04-12"와 같은 출력이 나온다고 가정합니다.

# 사용자 정의 형식으로 변경
{year, month, day} = Date.to_erl(date)
custom_date_string = "#{year}-#{month}-#{day}"
IO.puts custom_date_string  # "2023-4-12"와 같은 출력입니다.
```

## Deep Dive (심층 탐구)
날짜를 문자열로 변환하는 일은 오래된 문제입니다. Elixir에서는 `Date` 모듈을 사용하여 날짜 관련 작업을 수행합니다. `to_iso8601` 함수는 날짜를 ISO8601 국제 표준 형식으로 변환합니다.

또 다른 방법으로는 `to_erl` 함수를 사용하여 Erlang 날짜 포맷으로 변환한 다음, 원하는 형식의 문자열로 만들 수 있습니다. 

ISO8601 형식은 국제적으로 널리 인정되는 날짜 표현 방식이며, 데이터 교환 시 호환성이 높습니다. 사용자 정의 문자열은 더 유연하지만, 시스템 간 호환성 문제를 일으킬 수 있습니다.

## See Also (추가 정보)
- [Elixir Date Documentation](https://hexdocs.pm/elixir/Date.html)
- [ISO8601 Standard Information](https://en.wikipedia.org/wiki/ISO_8601)
