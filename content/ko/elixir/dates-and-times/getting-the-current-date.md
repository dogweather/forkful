---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:49.133164-07:00
description: "Elixir\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\
  \uB294 \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC758 \uB0A0\uC9DC\uC640 \uC2DC\uAC04 \uC815\
  \uBCF4\uC5D0 \uC811\uADFC\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD558\uBA70, \uB85C\
  \uAE45, \uB370\uC774\uD130 \uC2A4\uD0EC\uD551 \uB610\uB294 \uD604\uC7AC \uB0A0\uC9DC\
  \uB97C \uC54C\uC544\uC57C \uD558\uB294 \uAE30\uB2A5\uC5D0 \uD754\uD788 \uC0AC\uC6A9\
  \uB418\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uC2DC\uAC04\
  \uC5D0 \uBBFC\uAC10\uD55C \uC5B4\uD50C\uB9AC\uCF00\uC774\uC158\uC744 \uB9CC\uB4E4\
  \uACE0, \uC6F9 \uC5B4\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uB9AC\uD3EC\uD2B8\
  \uB098 \uD0C0\uC784\uC2A4\uD0EC\uD504\uB97C\u2026"
lastmod: '2024-03-13T22:44:54.737404-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294\
  \ \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC758 \uB0A0\uC9DC\uC640 \uC2DC\uAC04 \uC815\uBCF4\
  \uC5D0 \uC811\uADFC\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD558\uBA70, \uB85C\uAE45\
  , \uB370\uC774\uD130 \uC2A4\uD0EC\uD551 \uB610\uB294 \uD604\uC7AC \uB0A0\uC9DC\uB97C\
  \ \uC54C\uC544\uC57C \uD558\uB294 \uAE30\uB2A5\uC5D0 \uD754\uD788 \uC0AC\uC6A9\uB418\
  \uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uC2DC\uAC04\uC5D0\
  \ \uBBFC\uAC10\uD55C \uC5B4\uD50C\uB9AC\uCF00\uC774\uC158\uC744 \uB9CC\uB4E4\uACE0\
  , \uC6F9 \uC5B4\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uB9AC\uD3EC\uD2B8\uB098\
  \ \uD0C0\uC784\uC2A4\uD0EC\uD504\uB97C\u2026"
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Elixir에서 현재 날짜를 가져오는 것은 시스템의 날짜와 시간 정보에 접근하는 것을 포함하며, 로깅, 데이터 스탬핑 또는 현재 날짜를 알아야 하는 기능에 흔히 사용되는 작업입니다. 이 작업은 시간에 민감한 어플리케이션을 만들고, 웹 어플리케이션에서 리포트나 타임스탬프를 생성하는 작업과 같은 일에 필수적입니다.

## 방법:
Elixir의 표준 라이브러리는 `DateTime` 모듈을 통해 현재 날짜와 시간을 가져올 수 있습니다. Elixir가 Erlang VM (BEAM) 위에서 실행되므로, 시간 연산에 대해 기본적인 Erlang 기능을 활용합니다.

### Elixir의 표준 라이브러리 사용하기
Elixir는 UTC에서 현재 날짜와 시간을 얻기 위해 `DateTime.utc_now/0` 함수를 제공합니다.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**샘플 출력:**
```
~U[2024-02-05 19:58:40.925931Z]
```

현재 날짜만 얻으려면, 연도, 월, 일 구성 요소를 추출할 수 있습니다:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**샘플 출력:**
```
~D[2023-05-04]
```

### Timex 라이브러리 사용하기
더 복잡한 날짜 및 시간 요구 사항의 경우, Timex라는 인기 있는 타사 라이브러리를 사용할 수 있습니다. 먼저, `Timex`를 mix.exs 의존성에 추가합니다:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

의존성을 설치한 후(`mix deps.get`), Timex를 사용하여 현재 날짜를 얻을 수 있습니다:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**샘플 출력:**
```
~D[2023-05-04]
```

Timex는 날짜 및 시간 조작에 대한 광범위한 기능을 제공하여, 시간대, 형식 및 날짜 및 시간의 파싱을 처리할 때 특히 Elixir 애플리케이션에 강력한 추가 기능이 됩니다.

Elixir의 기본 기능과 Timex 라이브러리를 이해하고 활용함으로써, Elixir 애플리케이션에서 날짜와 시간을 쉽게 작업할 수 있으며, 정밀함과 용이함으로 애플리케이션의 필요에 맞춰 경험을 조정할 수 있습니다.
