---
title:                "C++: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## 왜 

JSON을 작업하는 것에 대해 설명하는 이유는 데이터를 깔끔하게 저장하고 전송하기 위해 사용되는 일반적인 형식이기 때문입니다. 이것은 데이터를 구조화하는 쉬운 방법이며 웹 애플리케이션과 API에서 널리 사용됩니다.

## 방법 

먼저, JsonCpp 라이브러리를 다운로드하고 프로젝트의 include 경로에 추가해야 합니다. 다음 코드를 사용하여 JSON 객체를 만들고 데이터를 저장할 수 있습니다.

```C++
#include <iostream>
#include "json/json.h"

int main() {
  // JSON 객체 생성
  Json::Value person; 
  
  // 객체에 데이터 저장
  person["name"] = "John Doe";
  person["age"] = 30;
  person["occupation"] = "Software Engineer";
  
  // 저장된 데이터 출력
  std::cout << person << std::endl;
  
  return 0;
}
```

위 코드에서는 새로운 JSON 객체를 만들고 데이터를 저장한 다음, 출력하였습니다. 아래는 위 코드의 출력 결과입니다.

```
{
  "name": "John Doe",
  "age": 30,
  "occupation": "Software Engineer"
}
```

또한 JSON 객체에서 특정 데이터를 가져오는 방법도 있습니다. 예를 들어, `person["name"]`을 사용하여 이름 데이터에 접근할 수 있습니다. 

## 딥 다이브

JSON을 작업하는 데에는 몇 가지 중요한 사항이 있습니다. 첫째, 데이터가 구조화되어야 합니다. 따라서 객체 내부에서 규칙을 준수하여 데이터를 저장하는 것이 중요합니다. 둘째, JSON은 텍스트 기반 형식이기 때문에 유효하지 않은 형식으로 작성된 데이터는 올바르게 해석되지 않습니다. 마지막으로, JsonCpp 라이브러리는 C++ 11 이상에서만 사용할 수 있습니다.

## 관련 링크

- [JsonCpp 라이브러리 다운로드](https://github.com/open-source-parsers/jsoncpp)
- [JsonCpp 문서](https://open-source-parsers.github.io/jsoncpp-docs/doxygen/index.html)
- [JSON 개요](https://ko.wikipedia.org/wiki/JSON)