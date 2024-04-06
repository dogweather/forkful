---
date: 2024-01-20 17:43:40.501829-07:00
description: "How to: (\uBC29\uBC95) \uC778\uD130\uB137 \uCD08\uAE30, \uC6F9\uD398\
  \uC774\uC9C0\uB294 \uC8FC\uB85C \uC815\uBCF4\uB97C \uC804\uB2EC\uD558\uB294 \uC218\
  \uB2E8\uC774\uC5C8\uC2B5\uB2C8\uB2E4. \uD604\uC7AC\uB294 HTTP \uD504\uB85C\uD1A0\
  \uCF5C\uC744 \uD1B5\uD574 \uB2E4\uC591\uD55C \uD615\uD0DC\uC758 \uB370\uC774\uD130\
  \uB97C \uAD50\uD658\uD569\uB2C8\uB2E4. \uB300\uC548\uC73C\uB85C\uB294 Wi-Fi \uBAA8\
  \uB4C8\uC744 \uC0AC\uC6A9\uD574 IoT \uB514\uBC14\uC774\uC2A4\uC758 \uAE30\uB2A5\uC744\
  \ \uD655\uC7A5\uD558\uB294 \uBC29\uBC95\uB3C4 \uC788\uC2B5\uB2C8\uB2E4. \uAD6C\uD604\
  \ \uC138\uBD80 \uC0AC\uD56D\uC5D0\uC11C\uB294 TCP \uC5F0\uACB0\uC744\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.870422-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uC778\uD130\uB137 \uCD08\uAE30, \uC6F9\uD398\uC774\uC9C0\
  \uB294 \uC8FC\uB85C \uC815\uBCF4\uB97C \uC804\uB2EC\uD558\uB294 \uC218\uB2E8\uC774\
  \uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
weight: 42
---

## How to: (방법)
```Arduino
#include <SPI.h>
#include <Ethernet.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
char server[] = "example.com";
EthernetClient client;

void setup() {
  Ethernet.begin(mac);
  Serial.begin(9600);
  delay(1000);

  if (client.connect(server, 80)) {
    client.println("GET /path HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  } else {
    Serial.println("Connection failed");
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
  
  if (!client.connected()) {
    Serial.println();
    Serial.println("disconnecting.");
    client.stop();
    while (true);
  }
}
```
Sample Output:
```
HTTP/1.1 200 OK
Date: Mon, 23 Jan 2023 12:28:53 GMT
Server: Apache/2.4.18 (Ubuntu)
Last-Modified: Sat, 21 Jan 2023 15:30:00 GMT
Content-Type: text/html
Content-Length: 1776

<!doctype html>
<html>
...
</html>
```

## Deep Dive (심층 분석)
인터넷 초기, 웹페이지는 주로 정보를 전달하는 수단이었습니다. 현재는 HTTP 프로토콜을 통해 다양한 형태의 데이터를 교환합니다. 대안으로는 Wi-Fi 모듈을 사용해 IoT 디바이스의 기능을 확장하는 방법도 있습니다. 구현 세부 사항에서는 TCP 연결을 이용하여 서버의 특정 포트로 요청을 전송하고 응답을 받는 과정을 자세히 이해할 필요가 있습니다.

## See Also (더 보기)
- Official Arduino Ethernet Library Documentation: [https://www.arduino.cc/en/Reference/Ethernet](https://www.arduino.cc/en/Reference/Ethernet)
