---
title:                "간단 인증과 함께 http 요청 보내기"
html_title:           "Arduino: 간단 인증과 함께 http 요청 보내기"
simple_title:         "간단 인증과 함께 http 요청 보내기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증과 함께 보내는 것에 참여하는 이유는 서버와 통신하고 데이터를 전송하기 위해서입니다.

## 하는 방법

기본 인증과 함께 HTTP 요청을 보내는 방법은 다음과 같습니다.

```Arduino
#include <ESP8266WiFi.h>
#include <WiFiClientSecure.h>

// Wi-Fi 연결 설정
const char* ssid = "YourNetworkName";
const char* password = "YourNetworkPassword";

// 요청할 서버의 URL과 포트 번호
const char* host = "www.example.com";
const uint16_t port = 443;

// Basic Authentication에 사용할 사용자 이름과 비밀번호
const char* username = "YourUsername";
const char* password = "YourPassword";

// HTTPS 클라이언트 생성
WiFiClientSecure client;

void setup() {
  // Wi-Fi 연결
  Serial.begin(9600);
  delay(10);
  Serial.println();
  Serial.print("Connecting to ");
  Serial.println(ssid);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("");
  Serial.println("WiFi connected.");

  // HTTPS 연결
  Serial.print("Connecting to ");
  Serial.println(host);
  if (!client.connect(host, port)) {
    Serial.println("Connection failed.");
    return;
  }

  // HTTP 헤더 생성
  String auth = username;
  auth.concat(":");
  auth.concat(password);
  char base64str[((auth.length() + 2) / 3) * 4] + 1;
  base64_encode(base64str, auth.c_str(), auth.length());
  String header = "Authorization: Basic ";
  header += base64str;
  header += "\r\n";

  // HTTPS 요청 전송
  client.println("GET / HTTP/1.1");
  client.print(header);
  client.println("Host: www.example.com");
  client.println("Connection: close");
  client.println();

  // 응답 내용 출력
  while (client.connected()) {
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      break;
    }
  }
  String line = client.readStringUntil('\n');
  Serial.println("Response:");
  Serial.println(line);
}

void loop() {

}
```

## 깊이 들어가기

기본 인증은 HTTP 헤더에 사용자 이름과 비밀번호를 인코딩하여 전송하는 인증 방식입니다. 이를 통해 서버는 사용자의 인증 정보를 확인하고, 유효한 요청인지 판단할 수 있습니다. 기본 인증은 안전하지 않으므로 HTTPS와 같은 보안 프로토콜을 사용하는 것이 좋습니다.

## 참고 자료

- [Arduino ESP8266WiFi 라이브러리 문서](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/client-secure-class.html)
- [기본 인증에 대한 HTTP 스펙](https://tools.ietf.org/html/rfc7617)
- [Base64 인코딩에 대한 위키피디아 글](https://ko.wikipedia.org/wiki/Base64)

---
### 참고 자료

- [Arduino ESP8266WiFi library documentation](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/client-secure-class.html)
- [HTTP specification for Basic Authentication](https://tools.ietf.org/html/rfc7617)
- [Wikipedia article on Base64 encoding](https://en.wikipedia.org/wiki/Base64)