---
title:                "웹 페이지 다운로드하기"
aliases:
- /ko/arduino/downloading-a-web-page.md
date:                  2024-01-20T17:43:40.501829-07:00
model:                 gpt-4-1106-preview
simple_title:         "웹 페이지 다운로드하기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/downloading-a-web-page.md"
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
