---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:40.745219-07:00
description: "YAML, \uC989 YAML Ain't Markup Language\uB294 \uC778\uAC04\uC774 \uC77D\
  \uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD3EC\uB9F7\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 XML\uC774\uB098 JSON\uC5D0\
  \ \uBE44\uD574 \uC77D\uAE30 \uC27D\uACE0 \uC774\uD574\uD558\uAE30 \uC26C\uC6B4 \uBB38\
  \uBC95\uC73C\uB85C \uC778\uD574 \uAD6C\uC131 \uD30C\uC77C, \uB370\uC774\uD130 \uB364\
  \uD504, \uACC4\uCE35\uC801 \uB370\uC774\uD130 \uC800\uC7A5\uC5D0 \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.699320-06:00'
model: gpt-4-0125-preview
summary: "YAML, \uC989 YAML Ain't Markup Language\uB294 \uC778\uAC04\uC774 \uC77D\uC744\
  \ \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD3EC\uB9F7\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 XML\uC774\uB098 JSON\uC5D0 \uBE44\
  \uD574 \uC77D\uAE30 \uC27D\uACE0 \uC774\uD574\uD558\uAE30 \uC26C\uC6B4 \uBB38\uBC95\
  \uC73C\uB85C \uC778\uD574 \uAD6C\uC131 \uD30C\uC77C, \uB370\uC774\uD130 \uB364\uD504\
  , \uACC4\uCE35\uC801 \uB370\uC774\uD130 \uC800\uC7A5\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML, 즉 YAML Ain't Markup Language는 인간이 읽을 수 있는 데이터 직렬화 포맷입니다. 프로그래머들은 XML이나 JSON에 비해 읽기 쉽고 이해하기 쉬운 문법으로 인해 구성 파일, 데이터 덤프, 계층적 데이터 저장에 사용합니다.

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
