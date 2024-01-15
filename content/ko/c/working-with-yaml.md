---
title:                "yaml로 작업하기"
html_title:           "C: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML을 사용하는 일에 참여하려는 이유는 무엇일까요? 이것에 대한 단 두 문장으로 설명하겠습니다. YAML은 사람이 쉽게 읽고 작성할 수 있는 형식의 데이터 교환 언어이기 때문에 프로그래머에게 매우 유용합니다.

## 하는 법

YAML 파일을 작성하고 읽는 방법을 알아보겠습니다. 먼저 YAML 파일을 열기 위해 `fopen()` 함수를 사용합니다. 그런 다음, YAML 파일 내용을 읽기 위해 `fread()` 함수를 사용합니다. 아래 예제 코드를 참고해주세요.

```C
FILE* file = fopen("example.yaml", "r");
char buffer[255];

while (fread(buffer, sizeof(buffer), 1, file) != 0) {
    printf("%s", buffer);
}

fclose(file);
```

위의 예제 코드를 실행하면 아래와 같은 출력이 나타납니다.

```C
name: John Smith
age: 29
occupation: Software Engineer
```

## 깊이 들어가보기

YAML 파일을 작성하고 읽는 것 외에도, YAML은 배열, 객체, 불리언 값 등 다양한 데이터 유형을 지원합니다. 또한 YAML은 주석을 사용할 수 있어서 설명이나 메모를 추가하는 데 유용합니다. YAML의 자세한 사용 방법은 YAML 공식 문서를 참고해주세요.

## 더 참고하기

- [YAML 공식 문서](https://yaml.org/)
- [C에서 YAML 사용하기](https://www.yolinux.com/TUTORIALS/LIBRARY/Yaml.html)