---
title:                "yaml로 작업하기"
html_title:           "Elixir: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

<!--한글 번역-->

## 왜 YAML을 사용하는가?

YAML은 Elixir에서 데이터를 효율적으로 저장하고 전달하는 데에 사용할 수 있는 간단하고 인간 친화적인 형식입니다. 따라서 YAML을 사용하는 것은 개발자들이 코드를 작성하고 데이터를 관리하는 데에 시간을 절약할 수 있는 좋은 방법입니다.

## 어떻게 사용할까?

YAML을 Elixir에서 사용하기 위해서는 먼저 이를 지원하는 라이브러리를 설치해야 합니다. 예를 들어, `YamlElixir` 라이브러리를 사용하면 다음과 같이 간단하게 YAML 파일을 파싱할 수 있습니다.

```Elixir
yaml = """
name: John
age: 30
occupation: Developer
"""

YamlElixir.parse(yaml)
```

위의 코드는 YAML 파일을 `:erlang` 모듈로 변환해주는 `YamlElixir.parse/1` 함수를 호출합니다. 이 함수의 반환값은 Elixir 데이터 형식으로 `{:ok, %{"name" => "John", "age" => 30, "occupation" => "Developer"}}`와 같이 튜플 형태로 반환됩니다.

또는, `Eyaml` 모듈을 사용하여 YAML 파일을 로드할 수도 있습니다.

```Elixir
Eyaml.load_file("config.yml")
```

로드된 YAML 파일은 `q` 마크와 `x` 마크를 포함하여 `{"key", "value"}` 형식의 튜플로 변환됩니다.

## 깊게 파헤치기

YAML 파일에는 다양한 데이터 형식을 저장할 수 있습니다. 이를 지원하기 위해 `YamlElixir` 라이브러리는 다음과 같은 기능을 제공합니다.

- 데이터 형식을 Elixir 타입으로 변환
- Elixir 타입을 YAML 형식으로 변환
- Elixir 맵을 YAML 파일로 저장
- YAML 파일 내의 데이터 검색
- YAML 파일에 데이터 추가 또는 수정하기

더 자세한 정보는 `YamlElixir` 공식 문서를 참고하시기 바랍니다.

## 또 다른 정보들

- [YamlElixir 공식 문서](https://hexdocs.pm/yaml_elixir/readme.html)
- [Eyaml 공식 문서](https://hexdocs.pm/eyaml/)