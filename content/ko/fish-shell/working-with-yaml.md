---
title:                "Fish Shell: yaml과 함께 작업하기"
simple_title:         "yaml과 함께 작업하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

# 왜 YAML을 사용해야 하는가?

YAML은 사람과 기계가 쉽게 읽고 작성할 수 있는 형식으로 데이터를 구조화하는 데 사용되는 텍스트 형식입니다. 따라서 YAML을 사용하면 다양한 용도로 데이터를 사용할 수 있으며, 데이터 작성과 관리가 쉬워집니다.

## 어떻게 시작하나요?

```Fish Shell```을 사용하여 YAML 파일을 읽고 쓰는 방법을 살펴보겠습니다. 먼저, YAML 파일을 읽어와서 해당 값을 변수에 할당하는 방법을 알아보겠습니다. 다음 코드를 ```config.fish``` 파일에 작성해보세요.

```Fish Shell
set -gx name (yaml eval name config.yml)
echo $name # config.yml 파일의 name 값 출력
```

위 코드에서 ```(yaml eval <key> <file>)```은 YAML 파일에서 해당 ```key```에 해당하는 값을 읽어와서 변수에 할당하는 명령어입니다. 이제, ```config.yml``` 파일을 아래와 같이 작성해보겠습니다.

```YAML
name: Fish Shell
version: 3.2.0
```

이제 터미널에서 ```source config.fish```를 입력하면, ```config.yml``` 파일에서 읽어온 ```name``` 값인 ```Fish Shell```이 출력됩니다.

또 다른 예시로, YAML 파일에 있는 배열 형식의 값을 변수에 할당해보겠습니다. 다음 코드를 ```config.fish``` 파일에 작성해보세요.

```Fish Shell
set -gx fruits (yaml eval fruits config.yml)
echo $fruits # config.yml 파일의 fruits 배열 값 출력
```

이제 ```config.yml``` 파일에 아래와 같이 배열 값을 추가해보겠습니다.

```YAML
fruits:
  - apple
  - banana
  - orange
```

터미널에서 ```source config.fish```를 입력하면, 배열 값인 ```apple banana orange```가 출력됩니다.

## YAML 조금 더 알아보기

YAML은 여러 가지 기능을 제공하기 때문에 모든 기능을 하나의 게시물에서 다루기는 어렵습니다. 하지만 붙여넣기 및 병합, 여러 파일 참조, 주석 처리 등 다양한 기능을 제공합니다. 더 자세한 내용은 아래 링크를 참조해주세요.

## 더 알아보기

[Fish Shell 공식 사이트](https://fishshell.com/)
[YAML 공식 사이트](https://yaml.org/)
[Fish Shell에서 YAML 사용하기 가이드](https://fishshell.com/docs/current/cmds/yaml.html)