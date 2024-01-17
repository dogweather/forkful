---
title:                "Json 처리하기"
html_title:           "C: Json 처리하기"
simple_title:         "Json 처리하기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation)은 데이터를 구조화하고 전송하기 위해 주로 사용되는 텍스트 형식입니다. 프로그래머들은 JSON을 사용하면 데이터를 간결하고 구조적으로 표현할 수 있으며, 다른 시스템 간의 데이터 교환을 더 쉽게 할 수 있습니다.

## How to:

```C
struct person {
    char name[50];
    int age;
    char occupation[50];
};

// JSON 생성 예제
struct person p1 = {"John", 25, "Programmer"};
char json[100];
sprintf(json, "{ \"name\": \"%s\", \"age\": %d, \"occupation\": \"%s\" }", p1.name, p1.age, p1.occupation);
printf("%s\n", json);

// JSON 파싱 예제
char jsonData[] = "{ \"name\": \"Sarah\", \"age\": 30, \"occupation\": \"Designer\" }";
struct person p2;
sscanf(jsonData, "{ \"name\": \"%[^\"]\", \"age\": %d, \"occupation\": \"%[^\"]\" }", p2.name, &p2.age, p2.occupation);
printf("Name: %s\nAge: %d\nOccupation: %s\n", p2.name, p2.age, p2.occupation);
```

## Deep Dive:

JSON은 프로그래밍 언어가 아니라 데이터 표현 형식으로, 원래는 JavaScript에서 사용하던 것이었습니다. 하지만 다양한 프로그래밍 언어에서 쉽게 사용할 수 있도록 널리 채택되고 있으며, 서버와 클라이언트 간의 통신에서 많이 사용됩니다.

JSON의 대안으로는 XML이 있습니다. XML에 비해 더 간결하고 가독성이 좋으며, 파싱 시간도 더 빠릅니다. 하지만 XML의 장점인 유효성 검사, 스키마 정의 등을 제공하지는 않습니다.

JSON은 키-값 쌍으로 이루어진 객체 형태의 데이터를 사용하여 자료구조를 표현합니다. C에서는 문자열과 정수, 실수 등의 기본 자료형을 사용하여 이를 구현할 수 있습니다. 또한 JSON을 사용하기 위해 여러 라이브러리가 제공되며, 이를 사용하면 더 쉽고 효율적으로 JSON을 다룰 수 있습니다.

## See Also:

- [JSON 공식 사이트](https://www.json.org/json-en.html)
- [C로 JSON 다루기](https://github.com/DaveGamble/cJSON)
- [JSON vs XML: 두 데이터 형식 비교](https://www.educba.com/json-vs-xml/)