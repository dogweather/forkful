---
title:                "JSON과 함께 일하기"
aliases:
- /ko/fish-shell/working-with-json.md
date:                  2024-02-03T19:22:50.956016-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Fish Shell에서 JSON을 다루는 것은 JSON 데이터를 파싱하고 생성하는 것을 포함하며, 애플리케이션 구성, API 상호작용, 그리고 커맨드 라인 워크플로우를 간소화하는 데 자주 사용되는 작업입니다. 웹과 어플리케이션 개발에서 JSON의 만연함을 고려할 때, 쉘에서 직접적으로 그것의 조작을 숙달하는 것은 프로그래머들의 자동화 및 데이터 처리 효율성을 상당히 향상시킬 수 있습니다.

## 어떻게 하나:

Fish Shell은 그 자체로는 JSON을 파싱하고 생성하기 위한 내장 유틸리티를 갖고 있지 않습니다. 하지만, `jq` 같은 타사 도구와의 손쉬운 통합을 제공합니다. `jq`는 강력하고 다재다능한 커맨드 라인 JSON 프로세서로, 간단하고 표현력이 뛰어난 언어를 사용하여 구조화된 데이터를 슬라이스하고, 필터링하고, 매핑하고, 변환할 수 있게 해줍니다.

### jq로 JSON 파싱하기
`jq`를 사용해 JSON 파일을 파싱하고 데이터를 추출하는 방법:

```fish
# 'data.json'이라는 이름의 JSON 파일이 있고, 내용이 {"name":"Fish Shell","version":"3.4.0"} 라고 가정합니다.
cat data.json | jq '.name'
# 출력 예시
"Fish Shell"
```

### jq로 JSON 생성하기
쉘 변수나 출력물에서 JSON 콘텐츠 생성:

```fish
# 변수에서 JSON 객체 생성하기
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# 출력 예시
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### JSON 컬렉션 필터링하기
`versions.json`이라는 이름의 파일에 객체의 JSON 배열이 있다고 가정해봅시다:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
이 배열에서 안정적인(Stable) 버전만 필터링하기 위해:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# 출력 예시
"3.1.2"
"3.4.0"
```

제공된 예시들은 Fish Shell에서 `jq`와 통합하여 JSON 작업을 수행하는 힘을 보여줍니다. 이러한 도구를 활용하는 것은 쉘 경험을 풍부하게 하여, 현대 데이터 포맷을 처리하는 데 있어 견고한 환경을 만들어냅니다.
