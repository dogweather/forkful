---
title:                "yaml로 작업하기"
html_title:           "Gleam: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?:
YAML 작업을 하는 것이 무엇인지 알려드리고, 프로그래머들이 이 작업을 왜 하는지에 대해 설명해드리겠습니다. 쉽게 말하면, YAML은 데이터를 저장하고 전송하기 위해 사용되는 형식입니다. 프로그래머들은 이를 사용하여 데이터를 구조화하고, 간결하며, 읽기 쉽게 만듭니다.

## 하는 법:
Gleam 코드 블록 안에 있는 코딩 예제와 출력 예제를 통해 어떻게 YAML을 사용하는지 알려드리겠습니다.
```
Gleam.config
    |> YAML.from_string("name: Gleam")
    |> YAML.to_string()
```
이 코드는 YAML 형식으로 name이 'Gleam'인 데이터를 만들고, YAML 형식으로 다시 변환하여 출력합니다.

## 깊이 파고들기:
YAML은 2001년에 처음 등장한 형식으로, 사람이 읽고 쓰기 쉽도록 디자인되었습니다. 리누스 토르발즈 등 프로그래밍의 거물들이 이 형식을 좋아하며 지속적으로 사용합니다. YAML 외에도 JSON과 XML과 같은 다른 데이터 형식이 있지만, YAML은 더 간결하면서도 읽기 쉽고 유연합니다. 이를 위해 거의 모든 프로그래밍 언어에서 지원하고 있기도 합니다.

## 더 보기:
YAML을 사용하는 다른 프로젝트를 살펴보려면 아래 링크를 확인해보세요!
- YAML 공식 문서: https://yaml.org/
- Gleam 문서의 YAML 섹션: https://gleam.run/documentation/standard_library.html#yaml