---
title:                "HTML 구문 분석"
html_title:           "Arduino: HTML 구문 분석"
simple_title:         "HTML 구문 분석"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/parsing-html.md"
---

{{< edit_this_page >}}

# 무엇이며, 왜 그러나요?:

HTML 파싱이란 무엇일까요? 프로그래머들이 이것을 하는 이유는 무엇일까요? HTML 파싱은 웹 페이지의 코드를 읽고, 원하는 데이터를 추출하는 과정입니다. 이를 통해 웹 서핑을 하거나 웹 크롤링 등 다양한 웹 프로그래밍 작업을 수행할 수 있습니다. 이를 통해 보다 자세한 내용을 얻어낼 수 있으며, 데이터를 쉽게 활용할 수 있습니다.

# 방법:

```Arduino
#include <SPI.h>
#include <Ethernet.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
char IP[] = "192.168.1.177";
EthernetClient client;
if (client.connect(IP, 80)) {
  client.println("GET /index.html HTTP/1.0");
  client.println();
}
while (client.connected()) {
  String line = client.readStringUntil('\n');
  if (line == "<h1>Hello World</h1>") {
    // do something
  }
}
client.stop();
```

위 예시 코드를 통해 Arduino를 사용하여 HTML을 파싱하는 방법을 살펴보겠습니다. Ethernet 라이브러리를 사용해 웹 페이지에 접속하고, ```readStringUntil()``` 함수를 사용하여 해당 페이지의 코드를 한 줄씩 읽어옵니다. 그리고 원하는 데이터를 찾을 때까지 반복하여 읽고, 원하는 작업을 수행합니다. 마지막으로 연결을 끊고 작업을 마칩니다.

# 더 알아보기:

HTML 파싱은 웹 프로그래밍에 있어 중요한 역할을 합니다. 기존에는 파서 라이브러리를 사용하여 HTML을 파싱했지만, 최근에는 CSS 선택자를 이용하는 방식이 더 선호되는 추세입니다. 이 방법을 사용하려면, ```ArduinoJson``` 라이브러리를 설치하고, 실제 웹 사이트의 코드를 분석하여 적절한 CSS 선택자를 사용해야 합니다.

# 관련 자료:

여러분도 아마 크롤링을 해보고 싶을 수 있습니다. 이를 위해서는 ```ArduinoJson```의 도움이 필요합니다. 또한, 웹 사이트의 HTML 코드를 분석해보고, CSS 선택자를 찾아 연습해볼 수 있습니다. 이를 통해 좀 더 많은 자료를 얻을 수 있을 것입니다.