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

## 왜

* JSON은 데이터를 쉽게 교환할 수 있는 포맷으로, C++ 프로그래머들에게 매우 중요합니다.
* 데이터를 처리하고 저장하는 데에 사용되는 스킬을 새로 배울 수 있어서 흥미로울 수 있습니다.

## 작동 방법

JSON을 사용하는 것은 간단합니다. 그러나 ASCII 코드로 남겨진 널문자를 제거하는 것과 같은 어려운 부분도 있습니다.

### JSON 읽기

아래의 예제는 JSON 파일 내용을 읽는 간단한 방법을 보여줍니다.

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <nlohmann/json.hpp> // JSON 라이브러리

int main() {
  // JSON 파일을 읽기 위해 ifstream 객체 생성
  std::ifstream file("example.json");
  
  // 파일 내용이 담기는 스트링 변수 선언
  std::string content;
  
  // 파일의 내용을 content에 저장
  file >> content;
  
  // json 변수에 파일 내용을 파싱하여 저장
  auto json = nlohmann::json::parse(content);
  
  // 키/값 쌍 출력하기
  for (auto& [key, value] : json.items()) {
    std::cout << key << ": " << value << std::endl;
  }
  
  return 0;
}
```

예제 파일의 내용이 아래와 같다면,

```json
{
  "name": "John",
  "age": 25,
  "hobbies": ["programming", "reading", "running"],
  "address": {
    "street": "123 Main St.",
    "city": "New York",
    "state": "NY"
  }
}
```

실행 결과는 다음과 같을 것입니다.

```text
name: John
age: 25
hobbies: ["programming", "reading", "running"]
address: {"street":"123 Main St.","city":"New York","state":"NY"}
```

### JSON 쓰기

아래의 예제는 C++에서 쉽게 JSON 파일을 쓰는 방법을 보여줍니다.

```C++
#include <iostream>
#include <fstream>
#include <nlohmann/json.hpp> // JSON 라이브러리

int main() {
  // JSON 파일을 쓰기 위해 ofstream 객체 생성
  std::ofstream file("example2.json");
  
  // data 변수에 저장할 데이터 선언
  nlohmann::json data;
  
  // 데이터 추가
  data["name"] = "Jane";
  data["age"] = 30;
  data["hobbies"] = {"painting", "yoga", "cooking"};
  data["address"]["street"] = "456 Maple Ave.";
  data["address"]["city"] = "Los Angeles";
  data["address"]["state"] = "CA";
  
  // 파일에 JSON 데이터 저장
  file << data << std::endl;
  
  return 0;
}
```

실행 결과, example2.json 파일에 다음과 같은 내용이 저장될 것입니다.

```json
{
  "name": "Jane",
  "age": 30,
  "hobbies": ["painting", "yoga", "cooking"],
  "address": {
    "street": "456 Maple Ave.",
    "city": "Los Angeles",
    "state": "CA"
  }
}
```

## Deep Dive

### JSON 구조

JSON은 다음과 같은 구조를 가지고 있습니다.

* 두 개의 큰 컨테이너: 객체(object)와 배열(array)
* 객체(object): key-value 쌍으로 이루어진 데이터 구조
* 배열(array): 하나의 데이터 타입으로 이루어진 목록 구조

### JSON 사용 시 유의할 점

1. 중괄호와 콜론 사용에 주의해야 합니다.
2. 문자열은 항상 쌍따옴표("")로 감싸야 합니다.
3. 배열의 요소는 콤마(,)로 구분되어야 합니다.
4. 파일을 읽고 쓸 때, ASCII