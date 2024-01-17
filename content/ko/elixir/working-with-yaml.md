---
title:                "Yaml 작업하기"
html_title:           "Elixir: Yaml 작업하기"
simple_title:         "Yaml 작업하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

# YAML와 함께하는 프로그래밍

## 무엇이고 왜?

YAML은 코드를 쉽게 읽고 작성할 수 있는 사람과 기계가 모두 이해할 수 있는 포맷을 가지고 있습니다. 프로그래머들은 YAML을 사용하여 데이터 구조를 정의하고 설정 파일을 작성하는 등 다양한 용도로 활용합니다.

## 어떻게 하나요?

```Elixir
username: "John"
password: "password123"
```

YAML에서는 위와 같이 `key:value` 형태로 데이터를 표현합니다. 쉽게 읽을 수 있고, 들여쓰기를 통해 데이터의 계층 구조를 표현할 수 있습니다. 위 예시에서는 `username`과 `password`라는 키와 그에 해당하는 값들이 정의되어 있습니다.

## 깊이 있는 알아보기

1. 역사적 배경: YAML은 2001년에 처음 등장했고, 인간이 쉽게 읽을 수 있도록 디자인된 것이 특징입니다.

2. 대안들: YAML 대신 XML, JSON 등의 다른 데이터 포맷을 사용할 수 있지만, YAML은 읽기 쉽고 구조적으로 명확하게 표현할 수 있어서 많은 프로그래머들이 선호합니다.

3. 구현 세부사항: YAML은 주로 들여쓰기로 계층 구조를 나타내기 때문에 들여쓰기에 규칙을 따라야 합니다. 또한 Elixir에서는 `YAML` 모듈을 사용하여 YAML 파일을 읽고 쓰는 기능을 제공합니다.

## 관련 자료

- [YAML 공식 사이트](http://yaml.org/)
- [Elixir YAML 모듈 문서](https://hexdocs.pm/elixir/YAML.html)