---
date: 2024-01-20 17:36:09.893910-07:00
description: "How to: (\uBC29\uBC95:) \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C\
  \ \uBCC0\uD658\uD558\uB294 \uC77C\uC740 \uC624\uB798\uB41C \uBB38\uC81C\uC785\uB2C8\
  \uB2E4. Elixir\uC5D0\uC11C\uB294 `Date` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uC5EC\
  \ \uB0A0\uC9DC \uAD00\uB828 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4. `to_iso8601`\
  \ \uD568\uC218\uB294 \uB0A0\uC9DC\uB97C ISO8601 \uAD6D\uC81C \uD45C\uC900 \uD615\
  \uC2DD\uC73C\uB85C \uBCC0\uD658\uD569\uB2C8\uB2E4. \uB610 \uB2E4\uB978 \uBC29\uBC95\
  \uC73C\uB85C\uB294 `to_erl`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.569638-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95:) \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\
  \uD558\uB294 \uC77C\uC740 \uC624\uB798\uB41C \uBB38\uC81C\uC785\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

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
