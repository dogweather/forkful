---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:29.267114-07:00
description: "\uBC29\uBC95: \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \ \uC5C6\uC774 \uC9C1\uC811 \uC811\uADFC."
lastmod: '2024-03-13T22:44:55.621466-06:00'
model: gpt-4-0125-preview
summary: "\uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC \uC5C6\uC774 \uC9C1\
  \uC811 \uC811\uADFC."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
서드파티 라이브러리 없이 직접 접근:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // YYYY-MM-DD 형식의 예제 날짜 문자열
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // 파싱된 구성요소로 DateTime 객체 초기화
  DateTime parsedDate(year, month, day);
  
  Serial.print("파싱된 날짜: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

샘플 출력:
```
파싱된 날짜: 2023/4/1
```

서드파티 라이브러리 사용 (*ArduinoJson* - JSON 응답에서 날짜를 얻는 것과 같은 더 복잡한 파싱 시나리오의 경우):

먼저, 아두이노 라이브러리 관리자를 통해 ArduinoJson 라이브러리를 설치합니다.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // JSON 응답 시뮬레이션
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // 날짜 문자열 추출
  const char* date = doc["date"];

  // 이전과 같이 문자열에서 날짜 파싱
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("JSON에서 파싱된 날짜: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

샘플 출력:
```
JSON에서 파싱된 날짜: 2023/7/19
```
