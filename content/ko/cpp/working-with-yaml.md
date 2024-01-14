---
title:                "C++: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

# 왜

YAML은 데이터를 효율적이고 쉽게 관리하기 위한 파일 형식입니다. 그래서 C++ 프로그래머들은 YAML을 사용하여 더 나은 코드를 작성하기 위해 알아야 합니다.

# 사용 방법

YAML을 사용하는 것은 간단합니다. 먼저 YAML 라이브러리를 프로젝트에 추가해야 합니다. 그 다음, YAML 파일을 읽고 쓰기 위해 필요한 함수를 호출하면 됩니다. 예를 들어, 다음 코드는 YAML 파일을 읽어서 간단한 출력을 생성합니다.

```C++
#include <iostream>
#include "yaml.h"

int main()
{
    YAML::Node config = YAML::LoadFile("example.yaml");

    std::cout << "Name: " << config["name"].as<std::string>() << "\n";
    std::cout << "Age: " << config["age"].as<int>() << "\n";
    std::cout << "Hobbies: \n";

    for (auto hobby : config["hobbies"])
    {
        std::cout << "- " << hobby.as<std::string>() << "\n";
    }

    return 0;
}
```

이 코드를 실행하면 다음과 같은 출력이 생성됩니다.

```
Name: Jane
Age: 25
Hobbies:
- Reading
- Cooking
- Hiking
```

# 깊이 파고들기

YAML을 다루는 데에는 몇 가지 중요한 개념이 있습니다. 첫째, 들여쓰기를 사용하여 계층적인 구조를 형성할 수 있습니다. 이를 통해 데이터를 구조적으로 정리하고 쉽게 파악할 수 있습니다. 둘째, YAML은 다양한 데이터 형식을 지원합니다. 문자열, 숫자, 불린 값 등 다양한 유형을 저장할 수 있습니다. 마지막으로, YAML은 주석을 지원하므로 코드의 의미를 설명하는 데 유용합니다.

# 이어보기

[YAML 시작하기(영문)](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/) - YAML에 대한 더 자세한 설명과 예제를 제공합니다.

[YAML 레퍼런스(영문)](https://yaml.org/spec/1.2/spec.html) - YAML 문법과 규칙에 대한 공식 문서입니다.