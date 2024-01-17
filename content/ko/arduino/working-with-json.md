---
title:                "json 작업하기"
html_title:           "Arduino: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

JSON을 다루는 것은 데이터를 다른 데이터 포맷으로 변환하거나 전송할 때 유용합니다. 이는 데이터를 보다 구조화하고 파싱하기 쉽게 만들어주며, 웹 애플리케이션에서 필수적인 작업입니다.

## 방법:

첫 번째로, JSON 라이브러리를 다운로드하고 설치해야 합니다. 아두이노에서는 다양한 라이브러리를 활용하여 JSON 데이터를 다룰 수 있습니다. 다음으로, JSON 코드 블록을 사용하여 원하는 데이터를 가져오고 이를 처리할 수 있습니다.

아래는 간단한 예시 코드입니다.

```Arduino
#include <ArduinoJson.h>

// JSON 데이터를 처리할 버퍼 크기 설정
const size_t bufferSize = 2 * JSON_OBJECT_SIZE(2) + 30;

// JSON 데이터를 저장할 버퍼 생성
StaticJsonBuffer<bufferSize> jsonBuffer;

// JSON 문자열 데이터
char* jsonString = "{\"name\":\"John\", \"age\":30}";

// JSON 오브젝트 생성
JsonObject& root = jsonBuffer.parseObject(jsonString);

// 필요한 데이터 가져오기
const char* name = root["name"];
int age = root["age"];

// 출력
Serial.print("Name: ");
Serial.println(name);
Serial.print("Age: ");
Serial.println(age);

```

위 코드의 출력은 다음과 같습니다.

```
Name: John
Age: 30
```

## 딥 다이브:

JSON은 원래 자바스크립트 프로그래밍 언어에서 사용하기 위해 만들어졌으며, 현재 웹 개발에서 널리 사용되고 있습니다. 따라서 JSON은 웹과 관련된 프로그래밍에서 필수적인 요소로 자리 잡고 있습니다. 아두이노에서는 다양한 라이브러리를 활용하여 JSON 데이터를 다룰 수 있으며, 그 외에도 다른 데이터 포맷으로 변환하는 방법이 있지만, JSON은 구조화 및 파싱 측면에서 가장 유리한 포맷 중 하나입니다.

## 관련 자료:

- [ArduinoJson 라이브러리 다운로드](https://arduinojson.org/)
- [JSON 공식 문서](https://www.json.org/json-en.html)