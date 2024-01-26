---
title:                "프로그래머를 위한 TOML 다루기"
date:                  2024-01-26T04:22:04.277138-07:00
model:                 gpt-4-0125-preview
simple_title:         "프로그래머를 위한 TOML 다루기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML을 사용한다는 것은 코드를 사용하여 TOML(Tom's Obvious, Minimal Language) 파일을 파싱하고 생성하는 것을 의미합니다. 프로그래머들은 명확한 의미론과 기존 데이터 유형과의 호환성 덕분에 TOML을 쉽게 읽을 수 있는 설정 파일과 데이터 직렬화에 사용합니다.

## 방법:
Gleam은 내장 TOML 지원이 없으므로 외부 라이브러리가 필요합니다. 예를 들어:

```gleam
// TOML 파싱 라이브러리가 있다고 가정:
import toml/{Parser, Encoder}

// TOML 내용 파싱
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// 파싱된 데이터 사용하기
match parsed {
  Ok(data) -> "데이터가 성공적으로 파싱되었습니다!"
  Error(_) -> "데이터 파싱 실패."
}

// Gleam 데이터 구조에서 TOML 내용 생성
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

샘플 출력:

```
데이터가 성공적으로 파싱되었습니다!
```

## 심층 분석
TOML은 2013년에 Tom Preston-Werner에 의해 발표되었습니다. 그 목적은 파일 설정을 위해 XML보다 읽기 쉽고 YAML보다는 덜 복잡하며 직관적인 것이었습니다. 단순함에도 불구하고, 구조화된 데이터를 위해 강력하며 명시적이고 이해하기 쉬운 문법을 제공합니다. 대안으로는 JSON, YAML, INI가 있지만, TOML의 최소주의적이고 명확한 문법이 설정 파일을 위한 승자로 자주 나타납니다. Gleam에서 TOML을 구현하는 것은 두 가지 주요 행동을 포함합니다: TOML을 네이티브 데이터 구조로 파싱하기와 네이티브 데이터 구조를 TOML로 직렬화하기. BEAM 언어와의 상호 운용성 덕분에 대부분의 Erlang 또는 Elixir용 TOML 라이브러리는 Gleam 프로젝트 내에서 원활한 통합을 보장하며 Gleam에서 사용할 수 있습니다.

## 참고
- TOML 언어 사양: [https://toml.io/en/](https://toml.io/en/)
- Erlang용 TOML 파서: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- GitHub상의 TOML: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)