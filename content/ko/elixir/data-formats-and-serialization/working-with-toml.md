---
date: 2024-01-26 04:20:58.077676-07:00
description: "TOML\uC744 \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740 Elixir\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC TOML(Tom's Obvious, Minimal Language) \uB370\uC774\uD130\
  \uB97C \uD30C\uC2F1\uD558\uACE0 \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 TOML\uC774 \uC77D\
  \uAE30 \uC27D\uACE0, \uD30C\uC2F1\uD558\uAE30 \uC27D\uACE0, \uD574\uC2DC \uB370\uC774\
  \uD130 \uAD6C\uC870\uC5D0 \uC798 \uB9E4\uD551\uB418\uAE30 \uB54C\uBB38\uC5D0 \uAD6C\
  \uC131 \uD30C\uC77C\uC744 \uB2E4\uB8E8\uAE30\u2026"
lastmod: '2024-03-13T22:44:54.756138-06:00'
model: gpt-4-0125-preview
summary: "TOML\uC744 \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740 Elixir\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC TOML(Tom's Obvious, Minimal Language) \uB370\uC774\uD130\uB97C\
  \ \uD30C\uC2F1\uD558\uACE0 \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\
  \uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 사용 방법:
먼저 mix 의존성에 TOML 파서를 추가합니다. 이 예제는 `toml-elixir`를 사용합니다:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

TOML 파일 읽기:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Elixir 데이터를 TOML로 변환:

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

샘플 출력:

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## 심층 탐구
TOML은 GitHub의 공동 창립자인 Tom Preston-Werner에 의해 구성 파일에서 사용하기 위해 만들어졌습니다. 이는 XML보다 더 단순하고 YAML보다 더 간결하게 설계되었으며 일관성을 유지합니다.

대안으로는 JSON, YAML, 그리고 INI 파일이 있으며, 각각 인간이 읽기 쉬움과 데이터 구조 호환성에서의 타협점을 가지고 있습니다. TOML은 표 데이터와 데이터의 중첩 그룹화를 명확하게 표현하는데 뛰어납니다.

Elixir에서 TOML 처리는 TOML 문자열을 Elixir 맵으로 변환하고 그 반대의 작업을 수행하는 디코딩 및 인코딩 라이브러리에 의존합니다. 파싱은 TOML의 문법 규칙과 일치시키고 이를 Elixir의 데이터 유형으로 변환하는 작업으로 이루어집니다. 인코딩은 그 반대로, Elixir의 데이터 유형을 유효한 TOML 문법으로 매핑합니다.

## 참고 자료
- TOML 언어: https://toml.io/en/
- `toml-elixir` GitHub 저장소: https://github.com/bitwalker/toml-elixir
- `toml-elixir` Hex 패키지 세부 정보: https://hex.pm/packages/toml_elixir
