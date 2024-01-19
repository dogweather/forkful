---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 하는가?
HTTP 요청을 보내는 것은 웹 서버와 상호작용하기 위한 방법입니다. 프로그래머들은 서버로부터 데이터를 가져오거나 상태를 확인하기 위해 HTTP 요청을 사용합니다.

## 이렇게 사용하세요:
Arduino를 이용해 HTTP GET 요청을 보내는 예제 코드와 결과를 확인해봅시다. 

```Arduino
#include <ESP8266WiFi.h>
 
WiFiClient client;
 
void setup() {
  Serial.begin(115200);
  
  // Wi-Fi 연결 설정
  WiFi.begin("your_ssid", "your_password");
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("WiFi에 연결 중...");
  }

  // HTTP 요청
  if (client.connect("example.com", 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  }
}
 
void loop() {
  while(client.available()) {
    char c = client.read();
    Serial.write(c);
  }
}
```
위 코드는 웹 서버에 HTTP GET 요청을 보내고, 응답을 받아 출력합니다.

## 깊이 이해하기
장치가 처음 생성될 때부터 HTTP는 웹 통신의 핵심이었습니다. Arduino와 같은 하드웨어에서 이를 구현하는 것은 대부분의 프로그래밍 언어가 제공하는 HTTP 라이브러리를 이용하는 것보다 복잡하지만, 그 어려움을 감수하면 웹 서버와 손쉽게 데이터를 교환할 수 있습니다. 

대안으로는 MQTT, CoAP와 같은 프로토콜이 있습니다. 이들은 더 적은 데이터를 사용하며, 실시간 통신에 더 적합하긴 하지만, 설치 및 설정이 더 복잡합니다. 

해당 코드에서는 ESP8266WiFi 라이브러리를 사용합니다. 이 라이브러리는 ESP8266 Wi-Fi 모듈을 쉽게 사용할 수 있게 도와줍니다.

## 참고 자료
더 자세한 정보를 위해 아래 링크들을 확인해보세요.

1. [Arduino 공식 홈페이지](https://www.arduino.cc/)
2. [HTTP - MDN Web Docs](https://developer.mozilla.org/ko/docs/Web/HTTP)
3. [ESP8266WiFi 라이브러리](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)