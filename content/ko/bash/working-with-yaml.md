---
title:                "yaml을 다루는 방법"
html_title:           "Bash: yaml을 다루는 방법"
simple_title:         "yaml을 다루는 방법"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜
 YAML을 활용한 작업을 하는 이유에 대해 2문장이내로 설명합니다.

 YAML은 인간이 쉽게 읽을 수 있는 형식으로 데이터를 표현하는 일반적인 파일 형식입니다. 이를 사용하면 복잡한 데이터 구조를 간단하고 이해하기 쉬운 방식으로 정리할 수 있습니다.

## 사용 방법

### YAML 설정 파일 만들기
쉽게 사용 가능한 `vi`에디터를 사용하여 YAML 설정 파일을 만듭니다.

```Bash
vi config.yaml
```

아래와 같은 형식으로 설정 파일을 작성할 수 있습니다.

```Bash
# 주석 추가 가능
first_name: John
last_name: Smith
age: 28
hobbies:
  - hiking
  - cooking
  - reading
```

### 데이터 읽어오기
YAML을 사용하여 설정 파일을 읽어오는 예제입니다.

```Bash
# 실행 파일 생성
touch read_yaml.sh
# 실행 파일 수정
vi read_yaml.sh
```

아래와 같은 코드를 추가합니다.

```Bash
#!/bin/bash

# YAML 설정 파일 로드
eval $(cat config.yaml | sed -e 's/:\s*/="/' -e 's/$/"/' | sed -n 's/^\(.*\)"\([^"]*\)"$/echo \1\2/p')
# 샘플 출력
echo "이름: $first_name $last_name"
echo "나이: $age"
echo "취미: "
for hobby in "${hobbies[@]}"
do
  echo "- $hobby"
done
```

설정 파일로부터 데이터를 읽어온 후 출력해주는 예제입니다.

### 스크립트 실행
스크립트를 실행하여 설정 파일로부터 데이터를 읽어옵니다.

```Bash
# 퍼미션 부여
chmod +x read_yaml.sh
# 스크립트 실행
./read_yaml.sh
```

아래와 같이 출력됩니다.

```Bash
이름: John Smith
나이: 28
취미:
- hiking
- cooking
- reading
```

## 깊게 들어가기
YAML은 복잡한 데이터 구조를 표현하기 적합한 형식입니다. 여러 계층으로 구성된 데이터를 정리하여 코드의 가독성을 높일 수 있습니다. 또한, 변수를 사용하여 간단하게 데이터를 수정할 수 있습니다.

## 참고 자료
- [YAML 공식 문서](https://yaml.org/)
- [Bash 공식 문서](https://www.gnu.org/software/bash/)
- [YAML을 사용하여 정렬하기](https://stackoverflow.com/questions/4247068/how-do-i-sort-a-yaml-file-with-shell-commands)
- [YAML을 사용하여 변수 사용하기](https://stackoverflow.com/questions/16969133/load-yaml-into-shell-script-with-variables#16969337)

## 참고하기
- [YAML 언어 버전 관리 시스템](https://yaml-language-server.readthedocs.io/en/latest/)
- [YAML을 사용하여 멀티 라인 수정하기](https://www.npmjs.com/package/yaml-multiline-editor)