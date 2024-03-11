---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:29.267114-07:00
description: "\uC544\uB450\uC774\uB178\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130\
  \ \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\
  \ \uD45C\uD604\uC73C\uB85C\uBD80\uD130 \uB0A0\uC9DC \uAD6C\uC131\uC694\uC18C(\uB144\
  , \uC6D4, \uC77C)\uB97C \uCD94\uCD9C\uD558\uACE0 \uBCC0\uD658\uD558\uC5EC \uC2A4\
  \uCF00\uCE58 \uB0B4\uC5D0\uC11C \uC2DC\uAC04 \uC720\uC9C0, \uBE44\uAD50 \uB610\uB294\
  \ \uC870\uC791\uC5D0 \uC0AC\uC6A9\uD560 \uC218 \uC788\uB294 \uD615\uC2DD\uC73C\uB85C\
  \ \uB9CC\uB4DC\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC2E4\uC2DC\uAC04 \uC2DC\uACC4, \uB85C\uAC70 \uB610\uB294 \uC6F9\
  \ API\uC640 \uC0AC\uC6A9\uC790\u2026"
lastmod: '2024-03-11T00:14:29.548301-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130\
  \ \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\
  \ \uD45C\uD604\uC73C\uB85C\uBD80\uD130 \uB0A0\uC9DC \uAD6C\uC131\uC694\uC18C(\uB144\
  , \uC6D4, \uC77C)\uB97C \uCD94\uCD9C\uD558\uACE0 \uBCC0\uD658\uD558\uC5EC \uC2A4\
  \uCF00\uCE58 \uB0B4\uC5D0\uC11C \uC2DC\uAC04 \uC720\uC9C0, \uBE44\uAD50 \uB610\uB294\
  \ \uC870\uC791\uC5D0 \uC0AC\uC6A9\uD560 \uC218 \uC788\uB294 \uD615\uC2DD\uC73C\uB85C\
  \ \uB9CC\uB4DC\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC2E4\uC2DC\uAC04 \uC2DC\uACC4, \uB85C\uAC70 \uB610\uB294 \uC6F9\
  \ API\uC640 \uC0AC\uC6A9\uC790\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

아두이노에서 문자열로부터 날짜를 파싱한다는 것은 텍스트 표현으로부터 날짜 구성요소(년, 월, 일)를 추출하고 변환하여 스케치 내에서 시간 유지, 비교 또는 조작에 사용할 수 있는 형식으로 만드는 것을 말합니다. 프로그래머들은 실시간 시계, 로거 또는 웹 API와 사용자 인터페이스로부터 입력을 처리할 때 이 작업을 자주 수행하며, 날짜가 읽을 수 있는 형식으로 제시될 수 있습니다.

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
