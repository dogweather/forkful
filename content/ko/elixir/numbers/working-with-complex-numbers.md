---
title:                "복소수 다루기"
aliases: - /ko/elixir/working-with-complex-numbers.md
date:                  2024-01-26T04:39:28.806754-07:00
model:                 gpt-4-0125-preview
simple_title:         "복소수 다루기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
복소수는 실수부와 허수부(`3 + 4i` 같은)를 가지고 있습니다. 복소수는 공학, 물리학, 그리고 특정한 컴퓨팅 문제에서 사용됩니다. 프로그래머들은 시뮬레이션, 신호 처리, 그리고 특정 유형의 수학 문제를 효율적으로 해결하기 위해 복소수를 사용합니다.

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
