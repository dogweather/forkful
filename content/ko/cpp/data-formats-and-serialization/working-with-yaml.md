---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:40.745219-07:00
description: "\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB098\uC694? C++\uC5D0\uC11C YAML\uC744\
  \ \uC0AC\uC6A9\uD558\uAE30 \uC704\uD55C \uC778\uAE30 \uC788\uB294 \uC120\uD0DD\uC740\
  \ `yaml-cpp` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC785\uB2C8\uB2E4. \uBA3C\uC800 `yaml-cpp`\uAC00\
  \ \uC124\uCE58\uB418\uC5B4 \uC788\uACE0 C++ \uD504\uB85C\uC81D\uD2B8\uC5D0 \uC81C\
  \uB300\uB85C \uC5F0\uACB0\uB418\uC5B4 \uC788\uB294\uC9C0 \uD655\uC778\uD558\uC138\
  \uC694. **YAML \uD30C\uC77C \uC77D\uAE30:**."
lastmod: '2024-03-13T22:44:55.699320-06:00'
model: gpt-4-0125-preview
summary: "C++\uC5D0\uC11C YAML\uC744 \uC0AC\uC6A9\uD558\uAE30 \uC704\uD55C \uC778\uAE30\
  \ \uC788\uB294 \uC120\uD0DD\uC740 `yaml-cpp` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC785\
  \uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 어떻게 사용하나요?
C++에서 YAML을 사용하기 위한 인기 있는 선택은 `yaml-cpp` 라이브러리입니다. 먼저 `yaml-cpp`가 설치되어 있고 C++ 프로젝트에 제대로 연결되어 있는지 확인하세요.

**YAML 파일 읽기:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Title: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

이와 같은 `config.yaml`을 가지고 있다면:

```yaml
title: "Example YAML"
```

위의 C++ 코드를 실행하면 다음을 생성합니다:

```
Title: Example YAML
```

**YAML 파일 쓰기:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Example YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

이 코드는 다음 내용을 가진 `output.yaml`을 생성합니다:

```yaml
title: Example YAML
```

이 예제들은 `yaml-cpp` 라이브러리를 사용하여 C++에서 YAML 파일을 읽고 쓰는 기본 소개를 제공합니다. 더 복잡한 구조와 사용 사례에 대해 탐구하려면 시퀀스, 태그와 같은 기능과 더 고급 직렬화 및 역직렬화 기술에 대해 `yaml-cpp` 문서를 확인하십시오.
