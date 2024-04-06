---
date: 2024-01-20 18:00:45.154112-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) HTTP \uAE30\uBCF8 \uC778\
  \uC99D\uC740 \uC624\uB798\uB41C \uBC29\uC2DD\uC774\uBA70, RFC7617\uC5D0\uC11C \uC815\
  \uC758\uD569\uB2C8\uB2E4. \uBCF4\uC548\uC740 \uAE30\uBCF8\uC774\uC5B4\uC11C \uC911\
  \uC694\uD55C \uB370\uC774\uD130\uB97C \uB2E4\uB8F0 \uB54C\uB294 \uB354 \uC548\uC804\
  \uD55C OAuth\uC640 \uAC19\uC740 \uB300\uC548\uC744 \uACE0\uB824\uD574\uC57C \uD569\
  \uB2C8\uB2E4. ESP8266WiFi \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 \uAE30\uBCF8 \uC778\
  \uC99D\uC744 \uC27D\uAC8C \uCC98\uB9AC\uD558\uB294 \uBA54\uC11C\uB4DC\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.871569-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) HTTP \uAE30\uBCF8 \uC778\uC99D\uC740\
  \ \uC624\uB798\uB41C \uBC29\uC2DD\uC774\uBA70, RFC7617\uC5D0\uC11C \uC815\uC758\uD569\
  \uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

## How to: (어떻게 하나요?)
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
const char* http_username = "user";
const char* http_password = "pass";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }

  HTTPClient http;
  http.begin("http://example.com/data"); // Your URL
  http.setAuthorization(http_username, http_password);

  int httpCode = http.GET();
  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(httpCode);
    Serial.println(payload);
  } else {
    Serial.println("Error on HTTP request");
  }

  http.end();
}

void loop() {
}
```

Sample Output:

```
200
{"data": "some response from server"}
```

## Deep Dive (심층 분석)
HTTP 기본 인증은 오래된 방식이며, RFC7617에서 정의합니다. 보안은 기본이어서 중요한 데이터를 다룰 때는 더 안전한 OAuth와 같은 대안을 고려해야 합니다. ESP8266WiFi 라이브러리는 기본 인증을 쉽게 처리하는 메서드 `setAuthorization`을 제공합니다. 이 메서드는 Base64로 사용자 이름과 비밀번호를 인코딩한 후 헤더에 추가하여 요청을 보냅니다.

## See Also (관련 자료)
- [Base64 Encoding and HTTP Basic Authentication](https://tools.ietf.org/html/rfc7617)
- [Wi-Fi Security and ESP8266](https://www.espressif.com/en/products/socs/esp8266/overview)
- [OAuth 2.0 for more secure authentication](https://oauth.net/2/)
