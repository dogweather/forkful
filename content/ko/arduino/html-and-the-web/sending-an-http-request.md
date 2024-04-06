---
date: 2024-01-20 17:59:12.645808-07:00
description: "How to: (\uBC29\uBC95) HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB824\uBA74\
  \ `ESP8266` \uB610\uB294 `ESP32`\uC640 \uAC19\uC740 \uC778\uD130\uB137\uC774 \uAC00\
  \uB2A5\uD55C Arduino \uD638\uD658 \uBCF4\uB4DC\uAC00 \uD544\uC694\uD569\uB2C8\uB2E4\
  . \uC544\uB798\uB294 \uAC04\uB2E8\uD55C HTTP GET \uC694\uCCAD \uC608\uC81C\uC785\
  \uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.248880-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB824\uBA74 `ESP8266`\
  \ \uB610\uB294 `ESP32`\uC640 \uAC19\uC740 \uC778\uD130\uB137\uC774 \uAC00\uB2A5\uD55C\
  \ Arduino \uD638\uD658 \uBCF4\uB4DC\uAC00 \uD544\uC694\uD569\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (방법)
HTTP 요청을 보내려면 `ESP8266` 또는 `ESP32`와 같은 인터넷이 가능한 Arduino 호환 보드가 필요합니다. 아래는 간단한 HTTP GET 요청 예제입니다:

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }

  HTTPClient http;
  http.begin("http://yourserver.com/api/data");
  int httpCode = http.GET();

  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(httpCode);
    Serial.println(payload);
  } else {
    Serial.println("Error in HTTP request");
  }
  http.end();
}

void loop() {
  // nothing here
}
```

샘플 출력:
```
200
{"name":"Arduino","message":"Hello World"}
```

## Deep Dive (심층 분석)
HTTP 요청을 보내는 방법은 릴리즈된 초기 인터넷 시작부터 있어왔습니다. 이는 웹의 기본 통신 규약으로, `GET`, `POST`, `PUT` 등 다양한 메서드를 이용합니다. `ESP8266`과 `ESP32` 보드는 WiFi 기능 내장 및 HTTP 통신을 손쉽게 처리하는 것으로 인기가 높습니다. 대안으로 `Ethernet Shield`를 사용해 유선 연결을 통한 HTTP 요청도 가능합니다. 핵심은 `HTTPClient` 라이브러리를 사용하여 요청을 구성하고 응답을 처리하는 것입니다.

## See Also (추가 자료)
- Arduino 공식 도움말: https://www.arduino.cc/en/Guide
- ESP8266HTTPClient 라이브러리: https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266HTTPClient
- WiFi 관련 문제 해결하기: https://arduino-esp8266.readthedocs.io/en/latest/faq/a02-my-esp-crashes.html
- HTTP 메서드 이해하기: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods
