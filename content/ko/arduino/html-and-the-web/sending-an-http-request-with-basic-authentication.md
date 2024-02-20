---
date: 2024-01-20 18:00:45.154112-07:00
description: "HTTP request with basic authentication\uC740 \uC11C\uBC84\uC5D0 \uC815\
  \uBCF4\uB97C \uC694\uCCAD\uD558\uAC70\uB098 \uC81C\uCD9C\uD560 \uB54C \uC0AC\uC6A9\
  \uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\uB97C \uD3EC\uD568\uD569\uB2C8\
  \uB2E4. \uC774\uB807\uAC8C \uD558\uBA74 \uC11C\uBC84\uAC00 \uC0AC\uC6A9\uC790\uB97C\
  \ \uC778\uC99D\uD558\uC5EC \uB370\uC774\uD130 \uBCF4\uC548\uC744 \uC720\uC9C0\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.522001
model: gpt-4-1106-preview
summary: "HTTP request with basic authentication\uC740 \uC11C\uBC84\uC5D0 \uC815\uBCF4\
  \uB97C \uC694\uCCAD\uD558\uAC70\uB098 \uC81C\uCD9C\uD560 \uB54C \uC0AC\uC6A9\uC790\
  \ \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\uB97C \uD3EC\uD568\uD569\uB2C8\uB2E4\
  . \uC774\uB807\uAC8C \uD558\uBA74 \uC11C\uBC84\uAC00 \uC0AC\uC6A9\uC790\uB97C \uC778\
  \uC99D\uD558\uC5EC \uB370\uC774\uD130 \uBCF4\uC548\uC744 \uC720\uC9C0\uD560 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP request with basic authentication은 서버에 정보를 요청하거나 제출할 때 사용자 이름과 비밀번호를 포함합니다. 이렇게 하면 서버가 사용자를 인증하여 데이터 보안을 유지할 수 있습니다.

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
