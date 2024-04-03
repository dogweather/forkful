---
date: 2024-01-27 20:33:08.639212-07:00
description: "Elixir\uC5D0\uC11C \uB79C\uB364 \uC22B\uC790\uB97C \uC0DD\uC131\uD558\
  \uB294 \uAC83\uC740 \uBCF4\uC548 \uD1A0\uD070 \uC0DD\uC131, \uB370\uC774\uD130 \uC0D8\
  \uD50C\uB9C1, \uAC8C\uC784 \uC54C\uACE0\uB9AC\uC998 \uB4F1 \uC608\uCE21\uD560 \uC218\
  \ \uC5C6\uB294 \uACB0\uACFC\uAC00 \uD544\uC694\uD55C \uC560\uD50C\uB9AC\uCF00\uC774\
  \uC158\uC5D0 \uD544\uC218\uC801\uC778 \uD504\uB85C\uADF8\uB798\uBC0D \uC791\uC5C5\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC560\uD50C\uB9AC\
  \uCF00\uC774\uC158\uC5D0 \uBB34\uC791\uC704\uC131\uACFC \uBCC0\uB3D9\uC131\uC744\
  \ \uB3C4\uC785\uD558\uC5EC, \uC774\uB97C \uB354 \uB3D9\uC801\uC774\uACE0 \uACB0\uC815\
  \uB860\uC801\uC774\uC9C0 \uC54A\uAC8C \uB9CC\uB4DC\uB294 \uB370\u2026"
lastmod: '2024-03-13T22:44:54.716267-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uC5D0\uC11C \uB79C\uB364 \uC22B\uC790\uB97C \uC0DD\uC131\uD558\uB294\
  \ \uAC83\uC740 \uBCF4\uC548 \uD1A0\uD070 \uC0DD\uC131, \uB370\uC774\uD130 \uC0D8\
  \uD50C\uB9C1, \uAC8C\uC784 \uC54C\uACE0\uB9AC\uC998 \uB4F1 \uC608\uCE21\uD560 \uC218\
  \ \uC5C6\uB294 \uACB0\uACFC\uAC00 \uD544\uC694\uD55C \uC560\uD50C\uB9AC\uCF00\uC774\
  \uC158\uC5D0 \uD544\uC218\uC801\uC778 \uD504\uB85C\uADF8\uB798\uBC0D \uC791\uC5C5\
  \uC785\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 무엇 & 왜?

Elixir에서 랜덤 숫자를 생성하는 것은 보안 토큰 생성, 데이터 샘플링, 게임 알고리즘 등 예측할 수 없는 결과가 필요한 애플리케이션에 필수적인 프로그래밍 작업입니다. 프로그래머들은 애플리케이션에 무작위성과 변동성을 도입하여, 이를 더 동적이고 결정론적이지 않게 만드는 데 사용합니다.

## 방법:

Elixir에서 랜덤 숫자를 생성하기 위해 주로 사용하는 것은 이 목적을 위한 다양한 함수를 제공하는 `:rand` 모듈입니다. 시작하는 데 필요한 간단한 가이드는 다음과 같습니다:

먼저, 랜덤 숫자 생성기를 고유한 시작점으로 초기화하기 위해 시드를 설정하세요:

```elixir
:rand.seed(:exsplus)
```

범위 내에서 랜덤 정수를 생성하려면 다음을 사용하세요:

```elixir
random_integer = :rand.uniform(10) # 1과 10 사이의 숫자를 생성
IO.puts(random_integer)
```

0과 1.0 사이의 랜덤 실수를 위해서는:

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

실수에 대해 더 특정한 범위가 필요할 수 있으며, 이는 조금 더 계산이 필요합니다:

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

이 숫자들은 의사 랜덤이라는 것을 기억하세요; 이들은 시드와 알고리즘에 의해 결정되지만 대부분의 애플리케이션에 충분합니다.

## 심층 탐구

Elixir의 랜덤 숫자 생성 기능은 Erlang의 `:rand` 모듈에 의존하며, 이는 그것의 유산과 Erlang과의 밀접한 관계를 반영합니다. `:rand` 모듈은 오래된 `:random` 모듈을 대체하여 랜덤 숫자 생성을 위한 개선된 알고리즘을 제공합니다. 이는 `exsplus`를 기본 알고리즘으로 하지만 `exs64`, `exsl` 등과 같은 다른 알고리즘도 지원하며, 속도와 랜덤성 품질 측면에서 각각의 장단점이 있습니다.

Elixir(따라서 Erlang의) 랜덤 숫자 생성의 흥미로운 측면은 시드의 처리 방식에 있습니다. 시스템은 각 프로세스에 대해 별도의 시드 상태를 유지하여, 동시에 실행되는 프로세스들이 서로의 랜덤 숫자 시퀀스에 간섭하지 않도록 합니다. 이는 특히 동시 애플리케이션에서 유용하여, 분산 시스템에서 예측 가능성과 신뢰성을 보장합니다.

`:rand` 모듈이 대부분의 사용 사례에 충분하지만, 암호학적으로 안전한 랜덤 숫자가 필요한 애플리케이션은 다른 옵션을 고려해야 합니다. `crypto` 모듈은 `crypto:strong_rand_bytes/1`과 같은 함수를 제공하여 암호화 목적에 적합한 안전한 랜덤 데이터를 생성하도록 설계되었습니다. 이러한 대안은 토큰 생성, 암호화, 특정 유형의 인증 메커니즘과 같은 보안 관련 애플리케이션에 필수적입니다.
