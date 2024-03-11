---
date: 2024-01-20 17:43:40.501829-07:00
description: "\uC6F9\uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD558\uB294\
  \ \uAC83\uC740 \uADF8 \uB0B4\uC6A9\uC744 \uC778\uD130\uB137\uC5D0\uC11C \uC9C1\uC811\
  \ \uBC1B\uC544\uC624\uB294 \uD589\uC704\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC790\uB3D9\uC73C\uB85C \uB370\uC774\uD130\uB97C \uC218\uC9D1\
  \uD558\uAC70\uB098 \uC6D0\uACA9\uC73C\uB85C \uC7A5\uCE58\uB97C \uC81C\uC5B4\uD558\
  \uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.531492-06:00'
model: gpt-4-1106-preview
summary: "\uC6F9\uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD558\uB294 \uAC83\
  \uC740 \uADF8 \uB0B4\uC6A9\uC744 \uC778\uD130\uB137\uC5D0\uC11C \uC9C1\uC811 \uBC1B\
  \uC544\uC624\uB294 \uD589\uC704\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC790\uB3D9\uC73C\uB85C \uB370\uC774\uD130\uB97C \uC218\uC9D1\uD558\
  \uAC70\uB098 \uC6D0\uACA9\uC73C\uB85C \uC7A5\uCE58\uB97C \uC81C\uC5B4\uD558\uAE30\
  \ \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
웹페이지를 다운로드하는 것은 그 내용을 인터넷에서 직접 받아오는 행위입니다. 프로그래머들은 자동으로 데이터를 수집하거나 원격으로 장치를 제어하기 위해 이 기능을 사용합니다.

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
