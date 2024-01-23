---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML은 구성 파일에서 자주 사용되는 데이터 직렬화 형식입니다. 프로그래머들은 YAML을 사용하여 설정이나 데이터를 쉽고 읽기 쉽게 표현합니다.

## How to:
Elixir에서 YAML 파일을 다루려면 `yaml_elixir`라는 외부 라이브러리를 추가해야 합니다.

`mix.exs`에 의존성 추가:
```elixir
def deps do
  [
    {:yaml_elixir, "~> 2.5"}
  ]
end
```

의존성을 가져온 후, YAML 파일을 Elixir로 읽기:
```elixir
defmodule Example do
  def read_yaml do
    {:ok, result} = YamlElixir.read_from_file("config.yaml")
    result
  end
end
```

YAML 파일 `config.yaml`:
```yaml
default:
  app_name: AwesomeApp
  secret_key_base: SUPERSECRET
```

Elixir 코드를 실행하면:
```elixir
iex> Example.read_yaml()
%{"default" => %{"app_name" => "AwesomeApp", "secret_key_base" => "SUPERSECRET"}}
```

## Deep Dive:
- YAML(YAML Ain't Markup Language)은 JSON처럼 데이터를 저장하고 전송하기 위해 설계된 언어 독립적인 포맷입니다. 약간 더 읽기 쉬운 형태로 JSON의 대안으로 사용됩니다.
- YAML은 다수의 프로그래밍 언어에서 지원되고 임시 구성이나 배포 스크립트에 주로 쓰입니다.
- Elixir에서는 `yaml_elixir`과 같은 라이브러리를 통해 YAML과 상호 작용하지만, 이는 내부적으로 Erlang의 YAML 파서를 활용하여 구현됩니다.

## See Also:
- `yaml_elixir` GitHub 페이지: https://github.com/KamilLelonek/yaml-elixir
- YAML 공식 사이트: https://yaml.org
- JSON과 YAML 차이점 비교하는 기사: https://www.redhat.com/sysadmin/yaml-vs-json
