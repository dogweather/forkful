---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

현재 날짜를 가져오는 것은 시스템의 현재 날짜와 시간을 불러오는 작업입니다. 프로그래머들이 이것을 사용하는 이유는 시간에 따른 계산을 수행하거나 데이터를 시간에 따라 정렬하기 위해서입니다.

## 어떻게 하는가:

현재 날짜를 가져오는 방법은 매우 간단합니다. Elixir에서는 DateTime 모듈의 `utc_now/0` 함수를 사용하여 현재 날짜와 시간을 UTC 형태로 가져옵니다.

```Elixir
DateTime.utc_now() |> DateTime.to_string()
```

이 코드를 실행하면 현재 날짜와 시간이 문자열 형태로 출력됩니다. 아래는 예시 출력입니다.

```Elixir
"2022-08-07 14:00:35.920268Z"
```

## 딥 다이브

현재 날짜를 가져오는 기능은 컴퓨터 프로그래밍에서 매우 오래 동안 사용되어 왔습니다. 이기능은 기록을 정확하게 유지하는데 사용되며, 서브루틴의 실행 시간을 기록하거나, 사용자 인터페이스를 업데이트하는 데에도 이용됩니다.

Elixir의 경우 다양한 방법으로 날짜를 처리할 수 있습니다. DateTime 모듈 외에도 `Date`와 `Time` 모듈이 있으며, 각각은 날짜와 시간을 별도로 처리합니다.

Elixir의 DateTime은 ISO 8601 날짜 및 시간 표준을 준수하며, 기본적으로 UTC 형태로 반환합니다. 이는 우리가 서로 다른 지역에서 동일한 시간을 가지게 되지 않도록 해주는 중요한 기능입니다.

## 참고하기: 

더 많은 정보를 원한다면, Elixir 공식 문서를 참조하십시오:
- [DateTime 공식 문서](https://hexdocs.pm/elixir/DateTime.html)
- [Date 공식 문서](https://hexdocs.pm/elixir/Date.html)
- [Time 공식 문서](https://hexdocs.pm/elixir/Time.html)