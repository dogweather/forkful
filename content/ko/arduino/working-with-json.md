---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

JSON은 데이터를 저장하고 구조적으로 표현하는 데 사용되는 경량 포맷입니다. 아두이노에서 JSON을 다루는 이유는 웹 API로부터 데이터를 받거나 보낼 때 통신을 쉽게 하기 위함입니다.

## How to: (방법)

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // JSON 데이터 정의
  const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

  // JSON 파싱
  DynamicJsonDocument doc(1024);
  deserializeJson(doc, json);

  // 데이터 추출
  const char* sensor = doc["sensor"];
  long time = doc["time"];
  double latitude = doc["data"][0];
  double longitude = doc["data"][1];

  // 콘솔에 결과 출력
  Serial.print("Sensor: ");
  Serial.println(sensor);
  Serial.print("Time: ");
  Serial.println(time);
  Serial.print("Latitude: ");
  Serial.println(latitude, 6);
  Serial.print("Longitude: ");
  Serial.println(longitude, 6);
}

void loop() {
  // 반복 동작 없음
}
```

샘플 출력:
```
Sensor: gps
Time: 1351824120
Latitude: 48.756080
Longitude: 2.302038
```

## Deep Dive (심층 분석)

JSON, JavaScript Object Notation의 약자,는 2000년대 초반 웹에서 JavaScript 객체를 교환하기 위해 개발되었습니다. XML의 보다 간단한 대안으로 널리 채택되었습니다. 아두이노에서는 `ArduinoJson` 라이브러리를 이용하여 JSON을 파싱하고 생성할 수 있습니다. 라이브러리는 효율성과 사용 편의성을 위해 설계되었으며, 메모리 할당을 최소화합니다.

## See Also (참고 자료)

- ArduinoJson 라이브러리 문서: https://arduinojson.org/
- ArduinoJSON GitHub 레포지토리: https://github.com/bblanchon/ArduinoJson
- JSON 공식 웹사이트: https://www.json.org/json-en.html
