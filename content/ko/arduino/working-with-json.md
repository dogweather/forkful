---
title:                "JSON 작업하기"
html_title:           "Arduino: JSON 작업하기"
simple_title:         "JSON 작업하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-json.md"
---

{{< edit_this_page >}}

# 왜 JSON을 다루어야 할까요?
JSON (JavaScript Object Notation)은 데이터를 교환하고 저장하기 위한 간단한 형식입니다. 이를 이용하면 다양한 시스템과 언어간의 데이터 교환을 쉽게 할 수 있습니다. 또한 웹 서비스와 모바일 애플리케이션 개발에서도 널리 사용되는 형식입니다.

## 어떻게 이용할 수 있나요?
아래의 예시 코드를 참고하여 Arduino에서 JSON을 다루는 방법을 알아보세요.

```Arduino
#include <ArduinoJson.h>
void setup() {
  String json = "{\"name\":\"Bob\", \"age\":32}"; //JSON 형식의 문자열 생성하기
  DynamicJsonDocument doc(1024); //JSON 문서 생성하기
  deserializeJson(doc, json); //문자열을 JSON 문서로 변환하기
  const char* name = doc["name"]; //name 요소의 값을 가져오기
  int age = doc["age"]; //age 요소의 값을 가져오기
  Serial.println(name); //name 값 출력하기
  Serial.println(age); //age 값 출력하기
}
void loop() {

}
```

위 코드의 출력 결과는 다음과 같습니다.

```
Bob
32
```

## 더 깊게 알아보기
ArduinoJson 라이브러리에는 다양한 기능과 메소드가 있으며, 자세한 내용은 공식 문서를 참고하세요. 또한 Arduino와 다른 라이브러리 또는 시스템 간의 데이터 교환을 위해서는 JSON 형식을 정확하게 이해하고 사용할 수 있어야 합니다. 따라서 JavaScript와 같은 다른 언어도 함께 공부하는 것이 좋습니다.

# 더 알아볼만한 자료
- [ArduinoJson 라이브러리 공식 문서](https://arduinojson.org/)
- [JSON 기초 개념](https://www.json.org/json-ko.html)
- [JSON 형식의 예시 데이터](https://jsonplaceholder.typicode.com/)