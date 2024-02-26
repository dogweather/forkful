---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:16.887488-07:00
description: "JSON\uC744 \uC0AC\uC6A9\uD558\uB294 \uC791\uC5C5\uC740 JSON \uD615\uC2DD\
  \uC758 \uBB38\uC790\uC5F4\uC744 Elixir\uAC00 \uC870\uC791\uD560 \uC218 \uC788\uB294\
  \ \uB370\uC774\uD130 \uAD6C\uC870\uB85C \uD30C\uC2F1\uD558\uACE0, Elixir \uB370\uC774\
  \uD130 \uAD6C\uC870\uB97C \uB2E4\uC2DC JSON \uBB38\uC790\uC5F4\uB85C \uC9C1\uB82C\
  \uD654\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774\uB294 JSON\uC774\
  \ \uB2E8\uC21C\uD558\uACE0 \uC778\uAC04\uC774 \uC77D\uAE30 \uC26C\uC6B4 \uACBD\uB7C9\
  \uC758 \uD14D\uC2A4\uD2B8 \uAE30\uBC18 \uC5B8\uC5B4 \uB3C5\uB9BD\uC801\uC778 \uB370\
  \uC774\uD130 \uAD50\uD658\u2026"
lastmod: '2024-02-25T18:49:51.790644-07:00'
model: gpt-4-0125-preview
summary: "JSON\uC744 \uC0AC\uC6A9\uD558\uB294 \uC791\uC5C5\uC740 JSON \uD615\uC2DD\
  \uC758 \uBB38\uC790\uC5F4\uC744 Elixir\uAC00 \uC870\uC791\uD560 \uC218 \uC788\uB294\
  \ \uB370\uC774\uD130 \uAD6C\uC870\uB85C \uD30C\uC2F1\uD558\uACE0, Elixir \uB370\uC774\
  \uD130 \uAD6C\uC870\uB97C \uB2E4\uC2DC JSON \uBB38\uC790\uC5F4\uB85C \uC9C1\uB82C\
  \uD654\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uC774\uB294 JSON\uC774\
  \ \uB2E8\uC21C\uD558\uACE0 \uC778\uAC04\uC774 \uC77D\uAE30 \uC26C\uC6B4 \uACBD\uB7C9\
  \uC758 \uD14D\uC2A4\uD2B8 \uAE30\uBC18 \uC5B8\uC5B4 \uB3C5\uB9BD\uC801\uC778 \uB370\
  \uC774\uD130 \uAD50\uD658\u2026"
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

JSON을 사용하는 작업은 JSON 형식의 문자열을 Elixir가 조작할 수 있는 데이터 구조로 파싱하고, Elixir 데이터 구조를 다시 JSON 문자열로 직렬화하는 것을 포함합니다. 이는 JSON이 단순하고 인간이 읽기 쉬운 경량의 텍스트 기반 언어 독립적인 데이터 교환 형식으로, 웹 개발, API, 구성 파일에 필수적이기 때문입니다.

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
