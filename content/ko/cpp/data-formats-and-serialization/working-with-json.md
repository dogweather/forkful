---
title:                "JSON과 함께 일하기"
date:                  2024-02-03T19:22:08.807630-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
