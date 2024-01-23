---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:35:34.474949-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 날짜 파싱은 문자열 형태의 날짜를 프로그램이 이해하고 사용할 수 있는 날짜 데이터로 변환하는 것입니다. 이 작업은 데이터가 텍스트로 제공될 때 날짜와 시간을 처리, 비교, 정렬하기 위해 필요합니다.

## How to: (어떻게:)
Elixir에서 문자열로부터 날짜를 파싱하기 위해서는 `DateTime` 모듈을 사용합니다. 여기 간단한 예시가 있습니다:

```elixir
# Elixir에서 ISO8601 날짜 문자열을 파싱하기
{:ok, datetime} = DateTime.from_iso8601("2023-03-14T08:02:00Z")
IO.inspect(datetime)
```

출력:
```
#DateTime<2023-03-14 08:02:00Z>
```

커스텀 형식으로 파싱할 때는 `Timex`라는 서드파티 라이브러리를 사용할 수 있습니다:

```elixir
# 먼저 Timex 라이브러리를 추가합니다. mix.exs 파일에 {:timex, "~> 3.7"}을 추가하세요.
import Timex

# "dd-MM-yyyy" 형식의 문자열을 파싱하기
{:ok, datetime} = DateTime.parse("14-03-2023", "{0D}-{0M}-{YYYY}", :strftime)
IO.inspect(datetime)
```

출력:
```
#DateTime<2023-03-14 00:00:00Z>
```

## Deep Dive (심층 분석)
이전에는 Elixir에서 날짜와 시간을 다루는 기능이 기본적이었습니다. Elixir 1.3부터 `DateTime`, `Date`, `Time` 모듈이 도입되면서 날짜와 시간을 보다 쉽게 다룰 수 있게 되었습니다. `DateTime.from_iso8601`는 ISO8601 표준을 따르는 날짜 문자열을 파싱하는 내장 함수입니다. 복잡한 형식이나 지역화된 날짜 처리를 위해서는 `Timex`와 같은 외부 라이브러리가 필요합니다. `Timex`를 사용하면 다양한 날짜 형식을 쉽게 파싱하고 포매팅 할 수 있어 실무에서 흔히 사용됩니다.

## See Also (추가 정보)
- Elixir 공식 문서의 `DateTime` 모듈: https://hexdocs.pm/elixir/DateTime.html
- ISO8601 날짜 형식에 대한 위키백과 페이지: https://ko.wikipedia.org/wiki/ISO_8601
- Timex 라이브러리 문서: https://hexdocs.pm/timex/Timex.html
