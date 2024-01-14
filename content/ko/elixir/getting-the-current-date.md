---
title:                "Elixir: 현재 날짜 가져오기"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

날짜를 구하는 것은 프로그래밍에서 매우 일반적인 작업입니다. 현재 날짜를 일일이 입력하는 대신에 Elixir를 사용하면 자동으로 현재 날짜를 가져올 수 있습니다.

## 어떻게

먼저, `DateTime.utc_now()` 함수를 사용하여 현재의 UTC 날짜와 시간을 가져올 수 있습니다. 다음과 같은 코드를 사용할 수 있습니다:

```Elixir
DateTime.utc_now()
```

아마도 더 많은 옵션을 지정하고 싶을 수도 있을 것입니다. 예를 들어, 현재 지역의 날짜와 시간을 가져오고 싶다면 `DateTime.now()` 함수를 사용할 수 있습니다. 이러한 옵션에 대한 예시는 다음과 같습니다:

```Elixir
DateTime.now()
DateTime.now("Etc/UTC", :second)
```

결과는 다음과 같을 것입니다:

```
~U[2021-07-08 03:54:35.240000Z]
~U[2021-07-08 03:54:35Z]
```

## 더 깊게 들어가기

`DateTime` 모듈은 날짜 및 시간을 처리하는 많은 함수를 제공합니다. 이러한 함수 중 몇 가지를 살펴보겠습니다.

첫째, 현재 날짜의 시간 정보를 가져오는 `DateTime.now()` 함수를 사용하면 시간대에 관계없이 현재의 날짜와 시간을 가져올 수 있습니다. 또한 함수들을 조합하거나 옵션을 전달하여 날짜 및 시간을 조정할 수도 있습니다.

두 번째로, `DateTime.to_string()` 함수를 사용하여 날짜 및 시간을 문자열로 변환할 수 있습니다. 예를 들어, `DateTime.utc_now() |> DateTime.to_string`을 실행하면 `"2021-07-08 04:10:42Z"`와 같은 결과가 반환될 것입니다.

마지막으로, `DateTime.compare()` 함수를 사용하여 두 개의 날짜 및 시간을 비교할 수 있습니다. 예를 들어, 다음과 같은 비교를 할 수 있습니다:

```Elixir
DateTime.compare(~U[2021-07-09 00:00:00Z], DateTime.utc_now())
```

결과는 다음과 같을 것입니다:

```
:lt
```

이 외에도 더 많은 함수를 찾아보고 사용할 수 있습니다. `DateTime` 모듈에 대한 자세한 내용은 공식 Elixir 문서를 참조하시기 바랍니다.

## 참고

- Elixir DateTime 모듈 문서: https://hexdocs.pm/elixir/DateTime.html
- Elixir Date 모듈 문서: https://hexdocs.pm/elixir/Date.html
- Elixir Time 모듈 문서: https://hexdocs.pm/elixir/Time.html