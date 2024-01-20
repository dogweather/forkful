---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
YAML은 데이터를 저장하고 전송하기 위한 텍스트 기반 형식입니다. 프로그래머는 설정 파일, 데이터 교환, 메타데이터 등을 간단하고 이해하기 쉽게 표현하기 위해 사용합니다.

## How to: (방법)
Gleam에서 YAML을 처리하는 특정 라이브러리나 내장 기능은 없습니다. 대신, Erlang 또는 Elixir의 YAML 라이브러리를 래핑하여 활용할 수 있습니다. 예제 코드와 출력을 확인하세요:

```Gleam
// Gleam에서 YAML 처리 예제 코드가 없으므로, 대신 연동 방법만 간단히 설명합니다.
// Erlang YAML 라이브러리를 사용하는 예시입니다.
external type Yaml

// YAML 파싱 함수를 외부에서 가져오기
external fn parse(String) -> Result(Yaml, Nil) =
  "yaml_erlang_lib":parse_doc

fn main() {
  let data = """
  name: John Doe
  age: 30
  """

  let result = parse(data)
  case result {
    Ok(parsed) -> IO.println("성공적으로 파싱: ")
    Error(_) -> IO.println("파싱 실패")
  }
}
```
(실제 출력은 YAML 라이브러리와 설정에 따라 다를 수 있습니다.)

## Deep Dive (깊이 탐구)
YAML(“YAML Ain't Markup Language”의 재귀적 약어)은 2001년에 등장했습니다. JSON이나 TOML 같은 다른 데이터 형식과 비교하면, YAML은 읽기 쉽고 사람이 직접 수정하기도 편리합니다. Gleam에서 YAML을 활용하려면, 주로 Erlang이나 Elixir 라이브러리를 이용하는 방법이 있습니다. 이는 Gleam이 BEAM 가상 머신상에서 실행되기 때문이며, 그에 따라 Erlang 생태계를 자유롭게 활용할 수 있습니다.

## See Also (참고 자료)
- YAML 공식 사이트: https://yaml.org
- Erlang YAML 라이브러리: https://github.com/yaml/yaml-erlang
- Elixir에서 Gleam 사용하기: https://gleam.run/book/tour/interop-elixir.html

(링크된 웹사이트 및 라이브러리 정보는 콘텐츠 작성 시점을 기준으로 하며, 방문 시 최신 정보를 확인하세요.)