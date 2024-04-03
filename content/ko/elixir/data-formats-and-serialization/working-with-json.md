---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:16.887488-07:00
description: "\uBC29\uBC95: Elixir\uC5D0\uC11C\uB294 JSON \uD30C\uC2F1\uACFC \uC0DD\
  \uC131\uC744 \uC704\uD55C \uC778\uAE30 \uC788\uB294 \uC120\uD0DD\uC778 `Jason` \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uBA3C\uC800 `mix.exs`\uC5D0\uC11C \uD504\uB85C\uC81D\uD2B8\uC758 \uC758\uC874\
  \uC131\uC5D0 `Jason`\uC744 \uCD94\uAC00\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.752893-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uC5D0\uC11C\uB294 JSON \uD30C\uC2F1\uACFC \uC0DD\uC131\uC744 \uC704\
  \uD55C \uC778\uAE30 \uC788\uB294 \uC120\uD0DD\uC778 `Jason` \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 방법:
Elixir에서는 JSON 파싱과 생성을 위한 인기 있는 선택인 `Jason` 라이브러리를 사용할 수 있습니다. 먼저 `mix.exs`에서 프로젝트의 의존성에 `Jason`을 추가합니다:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

그 다음, 의존성을 가져오기 위해 `mix deps.get`을 실행합니다.

### JSON 파싱:
JSON 문자열을 Elixir 데이터 구조로 변환하려면:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# 출력: %{"name" => "John", "age" => 30}
```

### JSON 생성:
Elixir 맵을 JSON 문자열로 변환하려면:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# 출력: {"age":25,"name":"Jane"}
```

### 구조체와 함께 작업하기:
Elixir 구조체를 인코드하려면, 구조체에 대해 `Jason.Encoder` 프로토콜을 구현해야 합니다. 다음은 예시입니다:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# 출력: {"age":28,"name":"Mike"}
```

이 간단한 접근 방식은 여러 프로그래밍 환경에서의 데이터 교환을 용이하게 하는 Elixir 애플리케이션에 JSON 처리를 통합하는 데 시작점이 될 것입니다.
