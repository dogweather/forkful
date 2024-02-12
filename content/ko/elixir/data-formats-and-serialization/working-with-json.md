---
title:                "JSON과 함께 일하기"
aliases:
- /ko/elixir/working-with-json/
date:                  2024-02-03T19:22:16.887488-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
