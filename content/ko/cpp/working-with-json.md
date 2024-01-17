---
title:                "json 작업하기"
html_title:           "C++: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?: 
JSON을 다루는 것은 데이터를 교환하는 데 사용되는 형식 중 하나입니다. 대부분의 웹사이트 및 웹 어플리케이션에서 데이터를 교환하고 저장하는 데 사용됩니다. 프로그래머는 서로 다른 애플리케이션 간에 데이터를 쉽게 공유할 수 있도록 JSON을 사용합니다. 

## 방법: 
JSON을 다루는 간단한 예시를 살펴보겠습니다. 다음 예시에서는 JSON 형식의 데이터를 읽고 출력하는 간단한 코드를 살펴볼 수 있습니다. 

```C++
#include <iostream>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

int main() {
    // 예시로 사용할 JSON 데이터
    std::string json_data = R"(
        {
            "name": "John",
            "age": 25,
            "country": "USA"
        }
    )";

    // JSON을 파싱하여 객체로 변환
    json parsed_data = json::parse(json_data);

    // name 필드 출력
    std::cout << "Name: " << parsed_data["name"] << std::endl;

    // age 필드 출력
    std::cout << "Age: " << parsed_data["age"] << std::endl;

    // country 필드 출력
    std::cout << "Country: " << parsed_data["country"] << std::endl;

    return 0;
}
```

#### 출력:
```
Name: John
Age: 25
Country: USA
```

## 깊이 들어가기: 
JSON은 원래 JavaScript에서 사용하기 위해 개발되었습니다. 하지만 현재는 다양한 프로그래밍 언어에서 사용되고 있습니다. 또한 JSON 대신 XML이나 CSV와 같은 다른 형식을 사용하여 데이터를 교환할 수도 있지만, JSON은 더 간결하고 쉽게 이해할 수 있는 형식으로 많이 사용됩니다. C++에서 JSON을 사용하기 위해서는 nlohmann의 json 라이브러리를 사용할 수 있습니다. 

## 관련 정보 보기: 
- nlohmann/json 공식 홈페이지: https://github.com/nlohmann/json 
- JSON의 동작 방식에 대한 상세한 설명: https://www.json.org/json-ko.html
- JSON 대신 사용할 수 있는 다른 데이터 교환 형식: XML과 CSV