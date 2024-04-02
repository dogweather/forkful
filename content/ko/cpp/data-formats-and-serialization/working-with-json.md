---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.807630-07:00
description: "JSON(JavaScript Object Notation)\uC740 \uB370\uC774\uD130\uB97C \uC800\
  \uC7A5\uD558\uACE0 \uC804\uC1A1\uD558\uAE30 \uC704\uD55C \uACBD\uB7C9 \uD3EC\uB9F7\
  \uC73C\uB85C, \uC11C\uBC84\uC640 \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uAC04\
  \uC758 \uB370\uC774\uD130 \uAD50\uD658\uC744 \uC704\uD55C \uD6CC\uB96D\uD55C \uB9E4\
  \uCCB4\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 JSON\uC744\
  \ \uC0AC\uC6A9\uD558\uB294\uB370, \uC774\uB294 \uC778\uAC04\uC5D0\uAC8C \uC27D\uAC8C\
  \ \uC77D\uD788\uACE0, \uAE30\uACC4\uAC00 \uBD84\uC11D\uD558\uAE30\uC5D0 \uC9C1\uAD00\
  \uC801\uC774\uAE30\u2026"
lastmod: '2024-03-13T22:44:55.700798-06:00'
model: gpt-4-0125-preview
summary: "JSON(JavaScript Object Notation)\uC740 \uB370\uC774\uD130\uB97C \uC800\uC7A5\
  \uD558\uACE0 \uC804\uC1A1\uD558\uAE30 \uC704\uD55C \uACBD\uB7C9 \uD3EC\uB9F7\uC73C\
  \uB85C, \uC11C\uBC84\uC640 \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uAC04\uC758\
  \ \uB370\uC774\uD130 \uAD50\uD658\uC744 \uC704\uD55C \uD6CC\uB96D\uD55C \uB9E4\uCCB4\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 JSON\uC744 \uC0AC\
  \uC6A9\uD558\uB294\uB370, \uC774\uB294 \uC778\uAC04\uC5D0\uAC8C \uC27D\uAC8C \uC77D\
  \uD788\uACE0, \uAE30\uACC4\uAC00 \uBD84\uC11D\uD558\uAE30\uC5D0 \uC9C1\uAD00\uC801\
  \uC774\uAE30\u2026"
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 무엇이며 왜 사용하는가?

JSON(JavaScript Object Notation)은 데이터를 저장하고 전송하기 위한 경량 포맷으로, 서버와 웹 애플리케이션 간의 데이터 교환을 위한 훌륭한 매체입니다. 프로그래머들은 JSON을 사용하는데, 이는 인간에게 쉽게 읽히고, 기계가 분석하기에 직관적이기 때문입니다. 특히 인터넷을 통한 데이터 교환 또는 구성 설정이 필요한 애플리케이션을 작업할 때 그렇습니다.

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
