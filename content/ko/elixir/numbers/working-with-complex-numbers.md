---
date: 2024-01-26 04:39:28.806754-07:00
description: "\uC5B4\uB5BB\uAC8C: Elixir\uB294 \uB0B4\uC7A5\uB41C \uBCF5\uC18C\uC218\
  \uB97C \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC544\uC11C, \uC6B0\uB9AC\uB294 \uC9C1\
  \uC811 \uB9CC\uB4E4\uAC70\uB098 `ComplexNum` \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC5EC\uAE30 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uC0AC\uC6A9\uD55C \uAC04\uB2E8\uD55C \uC608\uC2DC\uAC00 \uC788\uC2B5\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.713269-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uB294 \uB0B4\uC7A5\uB41C \uBCF5\uC18C\uC218\uB97C \uAC00\uC9C0\uACE0\
  \ \uC788\uC9C0 \uC54A\uC544\uC11C, \uC6B0\uB9AC\uB294 \uC9C1\uC811 \uB9CC\uB4E4\uAC70\
  \uB098 `ComplexNum` \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
weight: 14
---

## 어떻게:
Elixir는 내장된 복소수를 가지고 있지 않아서, 우리는 직접 만들거나 `ComplexNum` 같은 라이브러리를 사용합니다. 여기 라이브러리를 사용한 간단한 예시가 있습니다:

```elixir
# ComplexNum이 설치되어 있다고 가정
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# 복소수를 생성하고 더합니다
c1 = {3, 4}   # 3 + 4i를 나타냅니다
c2 = {2, -3}  # 2 - 3i를 나타냅니다
result = ComplexMath.add(c1, c2)
IO.puts "결과는: #{inspect(result)}"
```

이것은 다음을 출력합니다:
```
결과는: {5, 1}
```

이것은 `3 + 4i`와 `2 - 3i`의 합이 `5 + 1i`임을 의미합니다.

## 심층 탐구
복소수는 음수의 제곱근을 처리할 수 없는 일반적인 숫자로는 해결할 수 없는 문제 때문에 역사 속에 등장했습니다. 17세기에 이르러서야 르네 데카르트와 제롤라모 카르다노 같은 수학자들 덕분에 복소수는 진지하게 다루어졌습니다.

Elixir에서는 복소수를 `{3, 4}` 같은 튜플로 자주 사용하거나, 바퀴를 다시 발명하지 않기 위해 전용 라이브러리를 사용합니다. 라이브러리는 보통 더 나은데, 허수 단위 'i' 때문에 복잡해지는 곱셈과 나눗셈 같은 세세한 부분을 처리합니다(참고: `i`의 제곱은 `-1`입니다).

## 참고자료
다음 리소스를 확인하세요:
- Elixir의 패키지 관리자, Hex를 위한 [ComplexNum 라이브러리](https://hex.pm/packages/complex_num).
- 고급 Elixir 주제와 연습문제를 위한 [Elixir School](https://elixirschool.com/en/).
- Elixir가 내부적으로 사용하는 [Erlang -- math 모듈](http://erlang.org/doc/man/math.html), 다른 수학적 필요사항을 위해.
