---
title:                "Elixir: 날짜를 문자열로 변환하는 방법"
simple_title:         "날짜를 문자열로 변환하는 방법"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 이유는 프로그램에서 날짜를 보여줄 때 편리하기 때문입니다.

## 하는 방법

```Elixir
Date.to_string(Date.utc_today())
```

날짜를 문자열로 변환하는 가장 간단한 방법은 `Date.to_string/1` 함수를 사용하는 것입니다. 위의 예시는 오늘의 날짜를 UTC 시간으로 변환한 뒤, 해당 날짜를 문자열로 변환하는 코드입니다.

```Elixir
Date.to_string(Date.utc_today(), {:short, :long})
# "11/11/2021, 2021년 11월 11일 목요일"
```

위의 예시처럼, `Date.to_string/2` 함수를 사용하면 자세한 형식을 지정할 수 있습니다. 첫 번째 인자는 변환할 날짜를, 두 번째 인자는 `{숏 형식, 롱 형식}` 형태로 지정하여 날짜와 요일 모두를 포함한 문자열을 생성할 수 있습니다.

## 딥 다이브

Elixir에서는 `Date`라는 모듈이 날짜와 관련된 함수들을 제공합니다. `Date` 모듈은 `to_string/1`과 `to_string/2`뿐만 아니라, `to_iso8601/1`, `to_erl/1`, `date_to_time/1` 등 다양한 함수를 제공합니다. 따라서 원하는 형식으로 날짜를 변환하기 위해선 `Date` 모듈을 자세히 이해하는 것이 중요합니다.

## 참고자료

- [Elixir Date 모듈 공식 문서](https://hexdocs.pm/elixir/Date.html)
- [Elixir Date와 Time 관련 팁](https://www.mokacoding.com/blog/elixir-date-and-time-tips/)
- [가벼운 쇼핑 목록 API를 Elixir로 구현하기](https://dev.to/nicknisi/building-a-lightweight-shopping-list-api-in-elixir-4gk8)