---
title:                "Elixir: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

누군가가 미래나 과거의 날짜를 계산하는 것에 참여하는 이유는 무엇일까요? 이는 일자와 날짜를 다루는 프로그래밍에서 매우 유용한 기능이기 때문입니다. 예를 들어, 출판 시스템에서 최신 책의 출간 날짜를 계산하거나 예약 시스템에서 미래의 이벤트 날짜를 계산하는 경우에 유용합니다.

## 하는 방법

Elixir에서 일자와 날짜를 계산하는 방법은 매우 간단합니다. `Date` 모듈을 사용하면 됩니다. 예를 들어, 현재 날짜를 가져오는 방법은 다음과 같습니다.

```
Date.utc_today()
```

이제 계산할 날짜를 지정할 수 있습니다. 다음은 `2019`년 `7`월 `1`일에서 `10`일 전 날짜를 계산하는 예제입니다.

```
iex> Date.utc_today() |> Date.advance(-10)
~D[2019-06-21]
```

또는 다음과 같이 `DateTime` 모듈을 사용하여 시간까지 고려할 수도 있습니다.

```
iex> DateTime.utc_now() |> DateTime.advance(days: 5, hours: 10)
~U[2019-06-30 10:00:00Z]
```

## 깊게 파고들기

가장 간단한 방식을 넘어서, Elixir에서는 일자와 날짜를 조작하는 여러 가지 유용한 함수들을 제공합니다. `DateTime` 모듈을 사용하여 원하는 만큼 여러 개의 시간 단위까지 고려할 수 있습니다. 또한 `Date` 모듈은 특정 날짜의 기간을 나타내는 `Date.Range` 타입을 지원합니다. 이를 활용하면 특정 기간에 해당하는 날짜들을 쉽게 찾아낼 수 있습니다.

더 자세한 내용은 [Elixir 문서](https://hexdocs.pm/elixir/Date.html)를 참고하시기 바랍니다.

## 관련 링크

- [Elixir Date 문서](https://hexdocs.pm/elixir/Date.html)
- [South Korea Elixir User Group](https://elixir-ko.github.io/)