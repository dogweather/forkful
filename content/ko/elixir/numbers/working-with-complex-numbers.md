---
date: 2024-01-26 04:39:28.806754-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218\uBD80\uC640 \uD5C8\uC218\uBD80\
  (`3 + 4i` \uAC19\uC740)\uB97C \uAC00\uC9C0\uACE0 \uC788\uC2B5\uB2C8\uB2E4. \uBCF5\
  \uC18C\uC218\uB294 \uACF5\uD559, \uBB3C\uB9AC\uD559, \uADF8\uB9AC\uACE0 \uD2B9\uC815\
  \uD55C \uCEF4\uD4E8\uD305 \uBB38\uC81C\uC5D0\uC11C \uC0AC\uC6A9\uB429\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC2DC\uBBAC\uB808\uC774\uC158, \uC2E0\
  \uD638 \uCC98\uB9AC, \uADF8\uB9AC\uACE0 \uD2B9\uC815 \uC720\uD615\uC758 \uC218\uD559\
  \ \uBB38\uC81C\uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uD574\uACB0\uD558\uAE30 \uC704\
  \uD574 \uBCF5\uC18C\uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.713269-06:00'
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218\uBD80\uC640 \uD5C8\uC218\uBD80(`3\
  \ + 4i` \uAC19\uC740)\uB97C \uAC00\uC9C0\uACE0 \uC788\uC2B5\uB2C8\uB2E4. \uBCF5\uC18C\
  \uC218\uB294 \uACF5\uD559, \uBB3C\uB9AC\uD559, \uADF8\uB9AC\uACE0 \uD2B9\uC815\uD55C\
  \ \uCEF4\uD4E8\uD305 \uBB38\uC81C\uC5D0\uC11C \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC2DC\uBBAC\uB808\uC774\uC158, \uC2E0\uD638\
  \ \uCC98\uB9AC, \uADF8\uB9AC\uACE0 \uD2B9\uC815 \uC720\uD615\uC758 \uC218\uD559\
  \ \uBB38\uC81C\uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uD574\uACB0\uD558\uAE30 \uC704\
  \uD574 \uBCF5\uC18C\uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
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
