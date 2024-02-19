---
aliases:
- /ko/elixir/handling-errors/
date: 2024-01-26 00:51:39.450052-07:00
description: "\uC5D0\uB7EC\uB97C \uCC98\uB9AC\uD55C\uB2E4\uB294 \uAC83\uC740 \uC608\
  \uC0C1\uCE58 \uBABB\uD55C \uC77C\uB4E4\uC5D0 \uB300\uC751\uD560 \uC218 \uC788\uB294\
  \ \uCF54\uB4DC\uB97C \uC791\uC131\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC2DC\uC2A4\uD15C\uC774 \uCDA9\
  \uB3CC\uD558\uB294 \uAC83\uC744 \uBC29\uC9C0\uD558\uACE0, \uBA38\uD53C\uC758 \uBC95\
  \uCE59\uC774 \uC791\uC6A9\uD560 \uB54C \uD504\uB85C\uADF8\uB7A8\uC774 \uC6B0\uC544\
  \uD558\uAC8C \uD68C\uBCF5\uD560 \uC218 \uC788\uB3C4\uB85D \uD558\uAE30 \uC704\uD574\
  \ \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:05.766702
model: gpt-4-1106-preview
summary: "\uC5D0\uB7EC\uB97C \uCC98\uB9AC\uD55C\uB2E4\uB294 \uAC83\uC740 \uC608\uC0C1\
  \uCE58 \uBABB\uD55C \uC77C\uB4E4\uC5D0 \uB300\uC751\uD560 \uC218 \uC788\uB294 \uCF54\
  \uB4DC\uB97C \uC791\uC131\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC2DC\uC2A4\uD15C\uC774 \uCDA9\uB3CC\
  \uD558\uB294 \uAC83\uC744 \uBC29\uC9C0\uD558\uACE0, \uBA38\uD53C\uC758 \uBC95\uCE59\
  \uC774 \uC791\uC6A9\uD560 \uB54C \uD504\uB85C\uADF8\uB7A8\uC774 \uC6B0\uC544\uD558\
  \uAC8C \uD68C\uBCF5\uD560 \uC218 \uC788\uB3C4\uB85D \uD558\uAE30 \uC704\uD574 \uC774\
  \uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

에러를 처리한다는 것은 예상치 못한 일들에 대응할 수 있는 코드를 작성하는 것을 의미합니다. 프로그래머들은 시스템이 충돌하는 것을 방지하고, 머피의 법칙이 작용할 때 프로그램이 우아하게 회복할 수 있도록 하기 위해 이를 수행합니다.

## 어떻게:

Elixir에서는 주로 패턴 매칭과 `case` 문을 사용하여 다른 결과들, 에러를 포함하여 처리합니다.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "0으로 나눌 수 없습니다."}
      _ -> {:ok, a / b}
    end
  end
end

# 성공적인 나눗셈
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2는 #{result}입니다")

# 0으로 나누려는 시도
{:error, reason} = Example.divide(10, 0)
IO.puts("에러: #{reason}")
```

샘플 출력:
```
10 / 2는 5.0입니다
에러: 0으로 나눌 수 없습니다.
```

이 Elixir 코드를 실행하면, 입력에 따라 성공적인 나눗셈 결과나 에러 메시지를 얻을 수 있습니다. 여기서는 충돌이 없습니다!

## 자세히 알아보기

오래전에는 에러 처리가 주로 반환 값들을 확인하는 것으로 이루어졌습니다. 하지만 Elixir의 함수형 뿌리를 감안할 때, 우리는 `{:ok, value}` 또는 `{:error, reason}`과 같은 태그가 붙은 튜플과 패턴 매칭을 사용하는데, 이는 더 우아합니다.

Elixir에서는 에러처리를 위한 다른 방법들도 있습니다:

- **Elixir의 `try`와 `rescue`**는 명령형 언어에서의 전통적인 `try-catch`와 비슷하지만, Elixir에서는 명시성을 선호하기 때문에 덜 자주 사용됩니다.
- **Supervisors와 GenServers**, Elixir OTP 프레임워크의 일부로, 장애 내성에 관한 것입니다. 이들은 코드 프로세스를 감시하여 무언가 잘못되면 재시작할 준비가 되어 있습니다.

구현 측면에서, Elixir는 Erlang의 견고함을 이어받습니다. 에러를 패턴 매칭과 함수형의 장점을 사용하여 처리되어야 하는 메시지 타입 중 하나로 취급합니다.

## 또한 보기

Elixir에서의 에러 처리에 대해 더 알아보려면 아래 참조하세요:

- Elixir의 공식 가이드, [에러 처리](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- [프로세스와 OTP에 대해](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) 더 알아보세요.
- Elixir 포럼에서는 질문을 할 수 있는 좋은 장소입니다: [https://elixirforum.com](https://elixirforum.com).
