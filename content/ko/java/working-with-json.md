---
title:                "JSON으로 작업하기"
html_title:           "Java: JSON으로 작업하기"
simple_title:         "JSON으로 작업하기"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
JSON 작업이란 무엇이며, 프로그래머들이 왜 이것을 수행하는지 알려드리겠습니다. JSON(JavaScript Object Notation)은 데이터를 교환하기 위한 가벼운 형식으로, 자바스크립트와 비슷한 기능을 가지고 있습니다. 이것은 데이터를 보다 간단하게 관리하고 요청하기 위해 사용됩니다.

## 하는 방법:
아래 코드 블록에서 코딩 예제와 샘플 출력을 확인해보세요. JSON을 사용하여 데이터를 생성하고 처리하는 간단한 예제를 보여드리겠습니다.

```Java
// JSON 데이터 생성
JSONObject json = new JSONObject();

// 데이터 추가
json.put("name", "John");
json.put("age", 28);
json.put("major", "Computer Science");

// JSON 데이터 출력
System.out.println(json.toString());

// 출력 결과: {"name":"John","age":28,"major":"Computer Science"}
```

## 심층 탐구:
JSON이 무엇인지 이해하기 위해선, 이것의 역사적 배경과 대안에 대해 조금 더 알아볼 필요가 있습니다. JSON은 처음에는 XML이 대중적이었던 시기에 개발된 것으로, 간결하고 보다 가벼운 데이터 형식으로 인기를 끌게 되었습니다. 이제는 웹 서비스에서 빠르고 쉽게 데이터를 교환하기 위해 주로 사용되며, 다양한 프로그래밍 언어에서 지원하고 있습니다.

## 더 알아보기:
관련 정보를 확인하고 싶다면 아래 링크를 참고해보세요.
- [JSON 공식 사이트](https://www.json.org/json-ko.html)
- [Java에서 JSON 데이터 다루기](https://www.tutorialspoint.com/java_json/java_json_tutorial.pdf)
- [JSON vs XML: 무엇이 더 좋을까?](https://www.ibm.com/developerworks/library/ws-xmljava/)