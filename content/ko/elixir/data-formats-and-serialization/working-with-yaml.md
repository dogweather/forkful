---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:14.567152-07:00
description: "\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB294\uAC00: Elixir\uB294 \uB0B4\
  \uC7A5\uB41C YAML \uC9C0\uC6D0\uC744 \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\
  \uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098, `yamerl`\uC774\uB098 `yaml_elixir`\uC640\
  \ \uAC19\uC740 \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\
  \uC5EC YAML\uC744 \uC791\uC5C5\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\uAE30\
  \uC5D0\uC11C\uB294 \uC0AC\uC6A9\uC758 \uC6A9\uC774\uC131\uACFC \uD3EC\uAD04\uC801\
  \uC778 \uAE30\uB2A5 \uB54C\uBB38\uC5D0\u2026"
lastmod: '2024-03-13T22:44:54.751355-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uB294 \uB0B4\uC7A5\uB41C YAML \uC9C0\uC6D0\uC744 \uD3EC\uD568\uD558\
  \uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 어떻게 사용하는가:
Elixir는 내장된 YAML 지원을 포함하고 있지 않습니다. 그러나, `yamerl`이나 `yaml_elixir`와 같은 제3자 라이브러리를 사용하여 YAML을 작업할 수 있습니다. 여기에서는 사용의 용이성과 포괄적인 기능 때문에 `yaml_elixir`에 초점을 맞출 것입니다.

먼저, mix.exs 의존성에 `yaml_elixir`을 추가하세요:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

그 다음, `mix deps.get`을 실행하여 새로운 의존성을 가져오세요.

### YAML 읽기
다음과 같은 간단한 YAML 파일 `config.yaml`이 있습니다:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

이 YAML 파일을 읽고 이를 엘릭서 맵으로 변환할 수 있습니다:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# 샘플 사용예
Config.read()
# 출력: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### YAML 쓰기
맵을 다시 YAML 파일로 쓰려면:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# 샘플 사용예
ConfigWriter.write()
# 이것은 지정된 내용으로 `new_config.yaml`을 생성하거나 덮어쓸 것입니다
```

`yaml_elixir`이 YAML 파일과 엘릭서 데이터 구조간의 직관적인 변환을 가능하게 해주어, YAML 데이터를 다뤄야 하는 엘릭서 프로그래머들에게 훌륭한 선택이 되고 있음을 알 수 있습니다.
