---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.807630-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: C++\uC5D0\uC11C\uB294 JSON\uC744 \uAE30\uBCF8\
  \uC801\uC73C\uB85C \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC9C0\uB9CC, nlohmann/json\uACFC\
  \ \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\
  \uC6A9\uD558\uBA74 \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uAE30\uBCF8 \uC791\uC5C5\uC744\
  \ \uC704\uD574 \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95\uC740 \uB2E4\uC74C\uACFC \uAC19\
  \uC2B5\uB2C8\uB2E4: \uC6B0\uC120, \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uC124\uCE58\
  \uB418\uC5B4 \uC788\uB294\uC9C0 \uD655\uC778\uD558\uC138\uC694. vcpkg\uB098 Conan\uACFC\
  \ \uAC19\uC740 \uD328\uD0A4\uC9C0\u2026"
lastmod: '2024-03-13T22:44:55.700798-06:00'
model: gpt-4-0125-preview
summary: "C++\uC5D0\uC11C\uB294 JSON\uC744 \uAE30\uBCF8\uC801\uC73C\uB85C \uC9C0\uC6D0\
  \uD558\uC9C0 \uC54A\uC9C0\uB9CC, nlohmann/json\uACFC \uAC19\uC740 \uC11C\uB4DC\uD30C\
  \uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uBA74 \uAC04\uB2E8\
  \uD569\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 사용 방법:
C++에서는 JSON을 기본적으로 지원하지 않지만, nlohmann/json과 같은 서드파티 라이브러리를 사용하면 간단합니다. 기본 작업을 위해 사용하는 방법은 다음과 같습니다:

우선, 라이브러리가 설치되어 있는지 확인하세요. vcpkg나 Conan과 같은 패키지 관리자를 사용한다면, 프로젝트에 `nlohmann/json`을 쉽게 추가할 수 있습니다.

### 문자열에서 JSON 파싱하기
```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // 문자열로 된 JSON 데이터
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // JSON 문자열 파싱
    auto jsonObject = nlohmann::json::parse(jsonData);

    // 데이터 접근
    std::cout << "Name: " << jsonObject["name"] << "\n"
              << "Age: " << jsonObject["age"] << "\n"
              << "City: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**샘플 출력:**

```
Name: John
Age: 30
City: New York
```

### JSON 생성하기
JSON 데이터를 생성하는 것도 마찬가지로 간단합니다; 단지 `nlohmann::json` 객체에 값을 할당하면 됩니다.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // JSON 객체 생성
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // JSON 객체를 문자열로 변환하고 출력
    std::string jsonString = jsonObject.dump(4); // 예쁘게 출력하기 위해 인자 4 사용
    std::cout << jsonString << std::endl;

    return 0;
}
```

**샘플 출력:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

이 예제들은 `nlohmann/json` 라이브러리를 사용하여 C++에서 JSON을 다루는 핵심 기능을 보여줍니다. 이 기본 사항들을 이용하면, 구성 파일부터 네트워크 애플리케이션에 이르기까지 다양한 애플리케이션에서 JSON을 파싱하고 생성할 수 있습니다.
