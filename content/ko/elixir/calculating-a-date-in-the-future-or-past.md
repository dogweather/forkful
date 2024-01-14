---
title:                "Elixir: 미래나 과거의 날짜 계산하기"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여하는 이유는 무엇인가요? 이것은 매우 유용한 도구입니다. 새로운 데이터베이스를 만들거나 다른 언어로 작성한 코드와 호환하기 위해서 날짜의 연산이 필요할 때가 있기 때문입니다.

## 하는 법

Elixir는 날짜를 계산하는 강력한 도구입니다. 아래의 코드 블록을 참조하세요.

### 날짜 더하기

우리가 현재 날짜다음에 30일을 더해야 할 때, 우리는 `Date.today` 함수를 사용합니다. 그리고 그 뒤에 `|>` 파이프 라인 연산자를 사용하여 `add` 함수를 호출합니다.

```Elixir
iex> Date.today() |> Date.add(30)
```

다음과 같은 결과를 얻을 수 있습니다.

```
~D[2022-01-16]
```

### 날짜 빼기

날짜에서 시간을 빼고 싶다면 `DateTime.now` 함수를 사용하면 됩니다. 그리고 그 뒤에 `|>` 파이프 라인 연산자를 사용하여 `add` 함수를 호출합니다.

```Elixir
iex> DateTime.now() |> DateTime.add(-2, :days)
```

다음과 같은 결과를 얻을 수 있습니다.

```
~U[2022-01-14 05:41:38.432389Z]
```

## 깊이 파고들기

Elixir에서 날짜를 계산하는 방법에는 많은 옵션이 있습니다. 예를 들어 날짜 형식을 바꾸고 싶다면 함수 매개변수에 대한 정보를 확인해야 합니다. 또한 전 세계적으로 사용되는 날짜 형식을 지원하기 위해 Elixir는 여러 가지 포맷을 제공합니다.

## 참고

- [Elixir 공식 문서](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Date 라이브러리](https://github.com/lau/date)
- [Elixir DateTime 라이브러리](https://github.com/antoinejaussoin/date-time)