---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
YAML은 "YAML Ain't Markup Language"의 약어로, 설정 파일, 데이터 교환 등에 쓰는 데이터 직렬화 포맷입니다. YAML은 JSON이나 XML보다 읽고 쓰기 쉬워 프로그래머들이 선호합니다.

## How to: (어떻게 하나요?)
```Bash
# YAML 파일 읽기 예제
yaml_file="config.yaml"
echo "name: John Doe" > $yaml_file
echo "occupation: Developer" >> $yaml_file
echo "Reading YAML file:"
yq e '.name' $yaml_file # yq를 사용해서 name 값 추출
yq e '.occupation' $yaml_file # occupation 값 추출
```

```Bash
# 출력 결과:
Reading YAML file:
John Doe
Developer
```

```Bash
# YAML 파일 만들기 예제
create_yaml.sh
#!/bin/bash
name="Jane Doe"
occupation="Data Analyst"
cat << EOF > user.yaml
name: $name
occupation: $occupation
EOF
echo "YAML file 'user.yaml' created with content:"
cat user.yaml
```

## Deep Dive (깊은 탐색)
YAML은 2001년 Clark Evans, Ingy döt Net, Oren Ben-Kiki에 의해 개발되었습니다. JSON, XML과 비교하면 YAML은 가독성이 높고, 다양한 프로그래밍 언어에서 지원합니다. yq나 PyYAML 같은 도구로 커맨드라인에서 YAML을 다룹니다. 또한, 대규모 시스템에서는 Ansible이나 Kubernetes 같은 오토메이션 툴에서 YAML을 설정 파일로 사용합니다.

## See Also (함께 보기)
- YAML 공식 웹사이트: https://yaml.org
- yq GitHub 페이지: https://github.com/mikefarah/yq
- PyYAML 문서: https://pyyaml.org/wiki/PyYAMLDocumentation
- YAML 관련 온라인 해석기: http://yaml-online-parser.appspot.com/
