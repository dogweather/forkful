---
title:                "yaml로 작업하기"
html_title:           "Bash: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML 작업이란 무엇인지 알아보고, 프로그래머들이 그것을 하는 이유를 알아보자. YAML은 더 쉽고 가독성이 높은 데이터 포맷이다. 이는 데이터를 더 효율적으로 저장하고 전달하기 위해 사용된다.

## 어떻게:

YAML은 Bash 환경에서도 사용할 수 있으며, 다음과 같은 방법으로 작업할 수 있다.

```Bash
# 파일 생성
cat << EOF > test.yml
name: John
age: 25
favorite_food: pizza
EOF

# 파일 내용 출력
yaml2json test.yml
```

이렇게 하면 YAML 파일이 생성되고, ```yaml2json``` 명령어를 사용하면 YAML 파일을 JSON 형식으로 변환하여 출력할 수 있다.

## 더 깊이 알아보기:

YAML은 2001년에 처음 등장하였고, 인간이 쉽게 읽고 쓸 수 있는 형식으로 데이터를 저장하기 위해 만들어졌다. YAML은 다른 형식으로 데이터를 표현하는 JSON과 비슷하지만, 좀 더 가독성이 좋고 작성하기 쉽다는 장점이 있다.

YAML 대신에 JSON을 사용할 수도 있지만, YAML은 좀 더 인간 친화적이라는 이유로 널리 사용되고 있다. YAML은 Python, Ruby 등 다양한 프로그래밍 언어에서도 사용할 수 있으며, 다양한 라이브러리와 툴도 지원하고 있다.

## 관련 자료:

https://yaml.org/

https://json.org/

https://python.org/doc/

https://ruby-doc.org/