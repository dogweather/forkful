---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:14.567152-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C,\
  \ \uAD6C\uC131 \uD30C\uC77C\uACFC \uC11C\uB85C \uB2E4\uB978 \uB370\uC774\uD130 \uAD6C\
  \uC870\uB97C \uAC00\uC9C4 \uC5B8\uC5B4 \uAC04\uC758 \uB370\uC774\uD130 \uAD50\uD658\
  \uC5D0 \uC790\uC8FC \uC0AC\uC6A9\uB418\uB294 \uC778\uAC04\uC774 \uC77D\uC744 \uC218\
  \ \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD45C\uC900\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uADF8\uAC83\uC758 \uB2E8\uC21C\uC131\
  \uACFC \uBCF5\uC7A1\uD55C \uACC4\uCE35\uC801 \uB370\uC774\uD130\uB97C \uC27D\uAC8C\
  \ \uD45C\uD604\uD560 \uC218\u2026"
lastmod: '2024-02-25T18:49:51.789068-07:00'
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C, \uAD6C\
  \uC131 \uD30C\uC77C\uACFC \uC11C\uB85C \uB2E4\uB978 \uB370\uC774\uD130 \uAD6C\uC870\
  \uB97C \uAC00\uC9C4 \uC5B8\uC5B4 \uAC04\uC758 \uB370\uC774\uD130 \uAD50\uD658\uC5D0\
  \ \uC790\uC8FC \uC0AC\uC6A9\uB418\uB294 \uC778\uAC04\uC774 \uC77D\uC744 \uC218 \uC788\
  \uB294 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD45C\uC900\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uADF8\uAC83\uC758 \uB2E8\uC21C\uC131\uACFC\
  \ \uBCF5\uC7A1\uD55C \uACC4\uCE35\uC801 \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uD45C\
  \uD604\uD560 \uC218\u2026"
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

YAML은 "YAML Ain't Markup Language"의 약자로, 구성 파일과 서로 다른 데이터 구조를 가진 언어 간의 데이터 교환에 자주 사용되는 인간이 읽을 수 있는 데이터 직렬화 표준입니다. 프로그래머들은 그것의 단순성과 복잡한 계층적 데이터를 쉽게 표현할 수 있는 능력 때문에 이를 사용합니다.

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
