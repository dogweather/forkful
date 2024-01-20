---
title:                "JSON 다루기"
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
JSON은 데이터 교환 형식이다. 쉽고, 읽기 좋은 구조로 데이터를 전송하고 저장할 때 사용한다. 프로그래머는 빠른 데이터 교환과 플랫폼 간 호환성을 위해 JSON을 쓴다.

## How to: (어떻게 하나요?)
C++로 JSON을 다뤄보자. `nlohmann/json` 라이브러리를 사용한다. 설치 후 코드 예제를 테스트하자.

설치:
```sh
pip install nlohmann-json
```

기본 JSON 생성:
```C++
#include <nlohmann/json.hpp>
#include <iostream>

using json = nlohmann::json;

int main() {
    // JSON 객체 생성
    json j;
    j["name"] = "Kim";
    j["age"] = 25;
    j["is_programmer"] = true;
    
    // JSON을 문자열로 변환
    std::string s = j.dump();
    
    // 결과 출력
    std::cout << s << std::endl;
}
```
출력:
```plaintext
{"age":25,"is_programmer":true,"name":"Kim"}
```

JSON 파일 읽기 및 쓰기:
```C++
#include <nlohmann/json.hpp>
#include <iostream>
#include <fstream>

using json = nlohmann::json;

int main() {
    // 파일에서 JSON 읽기
    std::ifstream i("example.json");
    json j;
    i >> j;
    
    // JSON 사용
    std::cout << "Name: " << j["name"] << std::endl;
    
    // JSON 파일로 쓰기
    std::ofstream o("new_example.json");
    o << j.dump(4); // 예쁜 출력을 위한 들여쓰기
}
```

## Deep Dive (심층 탐구)
JSON(JavaScript Object Notation)은 2001년에 Douglas Crockford가 소개했다. XML과 달리 더 적은 코드로 데이터를 나타낼 수 있다. C++에서 `nlohmann/json` 말고도 `RapidJSON`, `JsonCpp` 같은 라이브러리들도 있다. `nlohmann/json`은 쉬운 사용법과 표준 C++ 기능 때문에 인기가 있다. 표준 라이브러리에는 JSON 지원이 내장되어 있지 않다.

## See Also (더 보기)
- nlohmann/json GitHub 페이지: https://github.com/nlohmann/json
- JSON 표준 사양: https://www.json.org/json-en.html
- RapidJSON 라이브러리: http://rapidjson.org/
- JsonCpp 라이브러리: https://github.com/open-source-parsers/jsoncpp