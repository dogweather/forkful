---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:48.799043-07:00
description: "\uC5B4\uB5BB\uAC8C? Elixir\uB294 \uB9E4\uC6B0 \uAC15\uB825\uD558\uACE0\
  \ \uC0AC\uC6A9\uD558\uAE30 \uC26C\uC6B4 \uB0B4\uC7A5 \uD14C\uC2A4\uD2B8 \uD504\uB808\
  \uC784\uC6CC\uD06C\uC778 ExUnit\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uAE30\uBCF8\
  \ \uC608\uB294 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4: 1. Elixir \uD504\uB85C\
  \uC81D\uD2B8\uC758 `test` \uB514\uB809\uD1A0\uB9AC\uC5D0 \uC0C8 \uD14C\uC2A4\uD2B8\
  \ \uD30C\uC77C\uC744 \uC0DD\uC131\uD569\uB2C8\uB2E4. \uC608\uB97C \uB4E4\uC5B4,\
  \ `MathOperations` \uBAA8\uB4C8\uC744 \uD14C\uC2A4\uD2B8\uD55C\uB2E4\uBA74\u2026"
lastmod: '2024-03-13T22:44:54.726818-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uB294 \uB9E4\uC6B0 \uAC15\uB825\uD558\uACE0 \uC0AC\uC6A9\uD558\uAE30\
  \ \uC26C\uC6B4 \uB0B4\uC7A5 \uD14C\uC2A4\uD2B8 \uD504\uB808\uC784\uC6CC\uD06C\uC778\
  \ ExUnit\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 어떻게?
Elixir는 매우 강력하고 사용하기 쉬운 내장 테스트 프레임워크인 ExUnit을 사용합니다. 기본 예는 다음과 같습니다:

1. Elixir 프로젝트의 `test` 디렉토리에 새 테스트 파일을 생성합니다. 예를 들어, `MathOperations` 모듈을 테스트한다면 테스트 파일은 `test/math_operations_test.exs`가 될 수 있습니다.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # 덧셈 함수를 확인하기 위한 간단한 테스트 케이스입니다
  test "두 수의 덧셈" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

테스트를 실행하기 위해서는 터미널에서 `mix test` 명령어를 사용하세요. 만약 `MathOperations.add/2` 함수가 두 숫자를 올바르게 더한다면, 다음과 같은 출력을 볼 수 있습니다:

```
..

완료 시간 0.03초
1 테스트, 0 실패
```

외부 서비스나 API를 다루는 테스트의 경우 실제 서비스에 접속하지 않기 위해 `mox`와 같은 모의 라이브러리를 사용할 수 있습니다:

1. `mix.exs`에 `mox`를 의존성에 추가하세요:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # 기타 의존성...
  ]
end
```

2. 테스트 도우미(`test/test_helper.exs`)에 모의 모듈을 정의하세요:

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. 테스트 케이스에서 모의를 사용하세요:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # 이는 Mox가 예상대로 이 모의가 호출되었는지 확인하도록 지시합니다
  setup :verify_on_exit!

  test "API에서 데이터를 가져옵니다" do
    # 모의 응답 설정
    expect(HTTPClientMock, :get, fn _url -> {:ok, "모의 응답"} end)
    
    assert SomeAPIClient.get_data() == "모의 응답"
  end
end
```

`mix test`를 실행할 때, 이 설정은 실제 외부 의존성으로부터 단위 테스트를 격리시켜, 자체 코드의 동작에 집중할 수 있게 합니다. 이 패턴은 외부 서비스 상태나 인터넷 연결 여부에 관계없이 테스트가 빠르고 신뢰성 있게 실행되도록 보장합니다.
