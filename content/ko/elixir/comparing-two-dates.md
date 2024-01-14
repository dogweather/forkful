---
title:    "Elixir: 두 날짜 비교하기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜 비교를 하는 이유는 여러분의 프로그램에서 날짜 관련 데이터를 처리해야 할 때 발생합니다. 이를 통해 데이터를 비교하고 원하는 결과를 얻을 수 있습니다.

## 사용 방법

날짜를 비교하는 방법은 간단합니다. 우선 `Date` 모듈을 로드해야 합니다.

```
Elixir
iex> use Date
```

두 날짜를 비교하려면 `Date.compare/2` 함수를 사용하여 비교하고자 하는 날짜를 표시하면 됩니다.

```
Elixir
iex> Date.compare(~D[2020-01-01], ~D[2020-02-01])
:lt
```

위의 예시에서는 `~D[날짜]` 형식을 사용하여 날짜를 지정했습니다. `:lt`는 첫 번째 날짜가 두 번째 날짜보다 이전임을 나타냅니다. 또 다른 예시를 살펴보겠습니다.

```
Elixir
iex> Date.compare(~D[1990-10-13], ~D[1990-09-20])
:gt
```

위의 예시에서는 첫 번째 날짜가 두 번째 날짜보다 나중임을 나타냅니다. 또 다른 방법으로는 `Date.before?/2`와 `Date.after?/2` 함수를 사용하여 두 날짜가 이전인지 또는 나중인지 확인할 수 있습니다.

```
Elixir
iex> Date.before?(~D[2000-01-01], ~D[2000-01-02])
true

iex> Date.after?(~D[2020-12-31], ~D[2020-12-30])
true
```

## 깊이 있는 분석

`Date` 모듈에는 다양한 함수와 기능이 있어 날짜 비교를 좀 더 다양하게 할 수 있습니다. 자세한 내용은 공식 Elixir 문서를 참조하시기 바랍니다.

## 관련 자료

- [Elixir 문서: Date](https://hexdocs.pm/elixir/Date.html)
- [프로그래밍 언어 Elixir를 활용한 웹 개발](https://brunch.co.kr/@thruthesky/306)