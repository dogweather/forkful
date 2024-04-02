---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:48.268836-07:00
description: "JSON \uB610\uB294 JavaScript \uAC1D\uCCB4 \uD45C\uAE30\uBC95\uC740 \uAC00\
  \uBCBC\uC6B4 \uB370\uC774\uD130 \uAD50\uD658 \uD3EC\uB9F7\uC73C\uB85C, \uC544\uB450\
  \uC774\uB178 \uD504\uB85C\uC81D\uD2B8\uC5D0\uC11C \uB370\uC774\uD130 \uC800\uC7A5\
  \uC18C\uB098 \uC124\uC815 \uD30C\uC77C\uC5D0 \uC774\uC0C1\uC801\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uADF8\uAC83\uC758 \uB2E8\uC21C\uD568\uACFC\
  \ \uB2E4\uC591\uD55C \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD\uC5D0\uC11C\uC758\
  \ \uAC00\uB3C5\uC131 \uB54C\uBB38\uC5D0 \uC0AC\uC6A9\uD558\uBA70, \uC6F9 API\uB098\
  \ \uB2E4\uB978 \uC2DC\uC2A4\uD15C\uACFC\uC758 \uC6D0\uD65C\uD55C \uB370\uC774\uD130\
  \ \uAD50\uD658\uC744\u2026"
lastmod: '2024-03-13T22:44:55.639585-06:00'
model: gpt-4-0125-preview
summary: "JSON \uB610\uB294 JavaScript \uAC1D\uCCB4 \uD45C\uAE30\uBC95\uC740 \uAC00\
  \uBCBC\uC6B4 \uB370\uC774\uD130 \uAD50\uD658 \uD3EC\uB9F7\uC73C\uB85C, \uC544\uB450\
  \uC774\uB178 \uD504\uB85C\uC81D\uD2B8\uC5D0\uC11C \uB370\uC774\uD130 \uC800\uC7A5\
  \uC18C\uB098 \uC124\uC815 \uD30C\uC77C\uC5D0 \uC774\uC0C1\uC801\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uADF8\uAC83\uC758 \uB2E8\uC21C\uD568\uACFC\
  \ \uB2E4\uC591\uD55C \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD\uC5D0\uC11C\uC758\
  \ \uAC00\uB3C5\uC131 \uB54C\uBB38\uC5D0 \uC0AC\uC6A9\uD558\uBA70, \uC6F9 API\uB098\
  \ \uB2E4\uB978 \uC2DC\uC2A4\uD15C\uACFC\uC758 \uC6D0\uD65C\uD55C \uB370\uC774\uD130\
  \ \uAD50\uD658\uC744\u2026"
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 무엇 & 왜?

JSON 또는 JavaScript 객체 표기법은 가벼운 데이터 교환 포맷으로, 아두이노 프로젝트에서 데이터 저장소나 설정 파일에 이상적입니다. 프로그래머는 그것의 단순함과 다양한 프로그래밍 환경에서의 가독성 때문에 사용하며, 웹 API나 다른 시스템과의 원활한 데이터 교환을 가능하게 해주는 아두이노를 포함합니다.

## 방법:

아두이노에서 JSON을 사용하기 위해, `ArduinoJson` 라이브러리는 사용의 용이성과 효율성 때문에 인기 있는 선택입니다. 이를 통해 JSON 문자열을 파싱하고, 수정하며, 객체를 다시 JSON 문자열로 직렬화할 수 있습니다. 사용 방법은 다음과 같습니다:

1. **ArduinoJson 라이브러리 설치**: 아두이노 IDE의 라이브러리 관리자를 사용하여 "ArduinoJson"을 설치합니다.

2. **JSON 문자열 역직렬화**: JSON 문자열을 파싱하고 값들을 추출하는 방법입니다.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // JSON 문서에 맞게 크기 조정
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() 실패: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float latitude = doc["data"][0]; // 48.756080
  float longitude = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // 빈 루프
}
```

샘플 출력:

```
gps
1351824120
48.756080
2.302038
```

3. **JSON 문자열 직렬화**: 데이터로부터 JSON 문자열을 생성하는 방법입니다.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // 데이터에 맞게 크기 조정
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // 빈 루프
}
```

샘플 출력 (가독성을 위해 포맷팅됨):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

`ArduinoJson` 라이브러리를 효과적으로 사용함으로써, 아두이노 프로젝트는 복잡한 데이터 구조를 인간이 읽을 수 있는 형식으로 통신할 수 있어, 개발 및 웹 서비스와의 통합을 촉진합니다.
