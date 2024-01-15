---
title:                "yaml과 함께 작업하기"
html_title:           "C++: yaml과 함께 작업하기"
simple_title:         "yaml과 함께 작업하기"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML은 간단한 문법을 사용하여 데이터를 인간과 컴퓨터가 이해하기 쉬운 형태로 저장할 수 있는 파일 형식입니다. 따라서 YAML을 사용하여 데이터를 처리하는 것은 개발자들에게 시간과 노력을 절약해줍니다.

## 어떻게

YAML을 사용하기 위해서는 다음과 같은 두 가지 라이브러리가 필요합니다. 

1. YAML 헤더 파일을 포함시켜야 합니다.
2. YAML 파일을 읽고 쓰기 위한 함수가 포함된 라이브러리를 설치해야 합니다.

```C++
# include <yaml-cpp/yaml.h> // YAML 헤더 파일을 포함
# include <iostream>

int main() {
  // YAML 파일 읽기
  YAML::Node node = YAML::LoadFile("data.yml");

  // YAML 파일 쓰기
  YAML::Emitter out;
  out << YAML::BeginMap;
  out << YAML::Key << "name";
  out << YAML::Value << "John";
  out << YAML::EndMap;
  std::cout << "output: " <<std::endl << out.c_str();
  return 0;
}
```
코드 실행 결과:

```
output:
name: John
```

## 자세히 알아보기

YAML 파일은 다양한 데이터 타입을 지원합니다. 정수, 실수, 불리언, 문자열, 리스트, 맵 등을 저장할 수 있습니다. 또한 YAML은 주석을 포함할 수 있기 때문에 다른 개발자들과 협업할 때 유용합니다. 또한 YAML 파일의 구조는 들여쓰기를 통해 시각적으로 파악할 수 있기 때문에 가독성이 뛰어납니다.

## 더 알아보기

- YAML 공식 사이트: https://yaml.org/
- YAML-CPP 라이브러리: https://github.com/jbeder/yaml-cpp
- YAML 문법 가이드: https://yaml.org/spec/1.2/spec.html
- YAML 판독기: https://yaml.com/