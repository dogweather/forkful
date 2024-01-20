---
title:                "웹 페이지 다운로드하기"
html_title:           "Arduino: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 해야 할까요?

웹 페이지 다운로드란 웹 서버에서 HTML 문서 전체를 컴퓨터에 저장하는 것을 의미합니다. 프로그래머들은 이걸 이용해 온라인 정보들을 오프라인에서 사용하거나, 데이터 분석에 활용하기 위해 웹 페이지를 다운로드해요.

## 어떻게 할까요?

다음 Arduino 코드를 참조하세요. 이건 Arduino 이더넷 라이브러리를 사용하여 웹 페이지를 다운로드하는 방법을 보여줍니다.

```Arduino
#include <Ethernet.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
EthernetClient client;

void setup()
{
  Ethernet.begin(mac);
  Serial.begin(9600);
  delay(1000);
  downloadWebPage();
}

void loop()
{
}

void downloadWebPage() {
  client.stop();
  if (client.connect("www.example.com",80)) {
    client.println("GET / HTTP/1.0");
    client.println();
  }
  else{
    Serial.println("connection failed");
    return;
  }
  while(client.connected()){
    if(client.available()){
      char c = client.read();
      Serial.print(c);
    }
  }
}
```

## 심층 탐색

웹 페이지 다운로드는 인터넷 초기 단계에서부터 있었던 기능이며 현재까지도 다양한 응용을 위해 사용되고 있습니다. 저 위의 예제가 실시간으로 웹 페이지의 내용을 얻어오는 방법이지만, 보다 효율적인 방법이 요구될 때는 크롤러와 같은 자동화 도구로 동기화 문제를 해결하는 방안도 많이 사용합니다.

또한, 이 코드는 아두이노나 ESP8266과 같은 임베디드 시스템에서 간단한 HTTP 요청을 보내고, 응답을 파싱하는 경우에 유용하게 쓸 수 있습니다.

## 참고자료

아래 링크는 본 글에서 다룬 내용과 관련된 자료입니다.

1. [Arduino Ethernet Library](https://www.arduino.cc/en/Reference/Ethernet)
2. [Arduino Ethernet Client](https://www.arduino.cc/en/Reference/EthernetClient)
3. [Arduino에서 웹 페이지 다운로드](https://stackoverflow.com/questions/1538947/how-to-download-a-web-page-in-arduino)