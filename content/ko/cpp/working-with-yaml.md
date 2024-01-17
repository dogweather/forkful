---
title:                "yaml 작업하기"
html_title:           "C++: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## YAML이란 무엇이며, 왜 개발자들이 이를 사용할까요?
YAML은 인간과 기계가 모두 읽고 쓰기 쉬운 형식의 데이터 직렬화 언어입니다. 개발자들은 YAML을 사용하여 데이터를 저장하고 전송하기 위해 사용합니다.

## 사용 방법:
```C++
#include <iostream>
#include <yaml-cpp/yaml.h>

int main() {
    // YAML 데이터 생성
    YAML::Emitter out;
    out << "name" << "John"
        << "age" << 25;

    // YAML 데이터 출력
    std::cout << out.c_str();

    return 0;
}
```
출력:
```
name: John
age: 25
```

## 깊이 들어가기:
YAML은 오픈 소스 프로젝트인 라이브러리 LibYAML에서 시작되었습니다. 이제 해당 라이브러리는 유지보수되지 않지만, YAML 공식 사이트에서는 유용한 정보와 다른 라이브러리를 찾을 수 있습니다. 대안으로, C++에서 JSON과 XML과 같은 다른 데이터 형식을 사용할 수도 있지만, YAML은 읽기 쉽고 간결한 문법을 가지고 있으며 많은 개발자들이 선호하는 형식입니다. YAML은 C++ 뿐만 아니라 다른 프로그래밍 언어에서도 널리 사용됩니다.

## 관련 자료:
- [YAML 공식 사이트](https://yaml.org/)
- [LibYAML 사이트](https://pyyaml.org/)
- [공식 C++ YAML 라이브러리](https://github.com/jbeder/yaml-cpp)
- [YAML 소개 비디오](https://youtu.be/vnh6Gj55OeY)