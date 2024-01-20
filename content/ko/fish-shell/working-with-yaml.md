---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
YAML은 구성 파일, 메시지 교환, 데이터 저장 등을 위한 데이터 직렬화 양식입니다. 간결성과 가독성 때문에, 프로그래머들은 복잡한 설정이나 프로젝트의 메타데이터를 표현하기 위해 YAML을 사용합니다.

## How to: (어떻게 하나요?)
Fish Shell에서 YAML을 다루기 위한 기본적인 예시를 보여줍니다.

```Fish Shell
# YAML 파일 읽기 예제
# input.yaml 파일 내용: name: Fish Shell
set file_content (cat input.yaml)
echo $file_content
# name: Fish Shell

# YAML 파일 작성 예제
echo "language: Fish" > output.yaml
cat output.yaml
# language: Fish
```

## Deep Dive (심층 분석)
YAML(YAML Ain't Markup Language)은 XML이나 JSON과 비교했을 때, 인간이 읽고 쓰기 쉽도록 설계되었습니다. 특히, 공백을 사용하여 데이터 구조를 표현하는 부분은 코드를 더욱 깨끗하게 만들어줍니다. 하지만 Fish Shell에는 내장된 YAML 파서가 없어, 대체로 `yq` 같은 외부 도구를 사용합니다. `yq`는 `jq`의 YAML 버전으로 JSON처럼 YAML을 다루게 해 줍니다.

## See Also (참고자료)
- YAML 공식 사이트: https://yaml.org
- `yq` GitHub 저장소: https://github.com/kislyuk/yq
- YAML과 JSON 비교: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats