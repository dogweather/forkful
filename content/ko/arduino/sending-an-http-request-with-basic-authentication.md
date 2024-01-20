---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

HTTP 요청 전송은 인터넷에서 정보를 가져오는 방법입니다. 기본 인증(Basic Auth)를 사용하면 사용자의 이름과 비밀번호를 전송하여 보안 연결을 확립합니다.

## 어떻게 하는가?:

다음은 HTTP GET 요청을 보내는 기본적인 예입니다:

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "your_SSID";
const char* password = "your_PASSWORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting...");
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    http.begin("http://example.com");
    http.setAuthorization("user", "pass"); 
    int httpCode = http.GET();

    if (httpCode > 0) {  
      String payload = http.getString();
      Serial.println(payload);
    }
    http.end();
  }
  delay(30000);
}
```

## 더 깊게 알아보기:

HTTP 요청과 기본 인증의 사용은 웹과 IoT 시스템에서 정보를 안전하게 교환하기 위한 오래된 방법입니다. 바이너리 정보를 Base64로 인코딩하여 "user:pass"를 서버로 전송하는 방법을 사용합니다.

대안으로 OAuth가 대부분의 유형의 인증을 처리하는데 사용됩니다. 인증 토큰을 사용하여 사용자 이름과 비밀번호보다 안전하게 정보를 전송합니다.

이 구현에서는 ESP8266HTTPClient 라이브러리를 사용하여 HTTP 요청을 처리합니다. WiFi 상태가 연결된 상태인지 확인한 다음, HTTPClient 객체를 초기화하고, 사용자 이름과 비밀번호로 인증을 설정하고, GET 요청을 전송합니다.

## 함께 보기:

3. [OAuth에 대한 위키](https://ko.wikipedia.org/wiki/OAuth)
4. [Base64 인코딩에 대한 위키](https://ko.wikipedia.org/wiki/Base64)
5. [Arduino WiFi 라이브러리](https://www.arduino.cc/en/Reference/WiFi)