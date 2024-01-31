---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"

category:             "C++"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇을 왜?)
YAML은 데이터 표현을 위한 간단한 언어입니다. 프로그래머들은 설정 파일, 데이터 교환 등을 위해 YAML를 사용합니다.

## How to: (어떻게?)
라이브러리 선택: yaml-cpp
```C++
// YAML 이용 예제
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    // YAML 파일 읽기
    std::ifstream fin("config.yaml");
    YAML::Node config = YAML::Load(fin);

    // 설정 데이터 접근
    std::string name = config["name"].as<std::string>();
    int age = config["age"].as<int>();
    
    // 출력
    std::cout << "Name: " << name << ", Age: " << age << std::endl;
    
    // YAML 파일 쓰기
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "name" << YAML::Value << "Kim";
    out << YAML::Key << "age" << YAML::Value << 25;
    out << YAML::EndMap;

    // 파일에 저장
    std::ofstream fout("output.yaml");
    fout << out.c_str();

    return 0;
}
```
출력:
```
Name: Lee, Age: 34
```

## Deep Dive (심층 분석)
YAML은 "YAML Ain't Markup Language"입니다 (재귀적 약어). JSON, XML같은 다른 데이터 포맷에 비해 인간이 읽고 쓰기 쉽습니다. yaml-cpp는 C++에서 YAML을 파싱하고 출력하는 데 사용됩니다. C와 Python용 라이브러리도 있습니다.

## See Also (참고 자료)
- YAML 공식 사이트: [yaml.org](https://yaml.org)
- yaml-cpp GitHub 저장소: [github.com/jbeder/yaml-cpp](https://github.com/jbeder/yaml-cpp)
- YAML 퀵 스타트 가이드: [learnxinyminutes.com/docs/yaml](https://learnxinyminutes.com/docs/yaml/)
- YAML 사양: [yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
