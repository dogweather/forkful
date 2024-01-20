---
title:                "HTML 파싱"
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTML 파싱은 웹페이지의 데이터를 추출하기 위해 HTML 코드를 분석하는 작업입니다. 이는 프로그래머들이 동적으로 사이트의 내용을 읽고 분석하게 해줍니다.

## 어떻게:

```Arduino
#include <ArduinoHttpClient.h>
#include <ArduinoJson.h>

HttpClient client = HttpClient(WiFi, server, port);
client.get(uri);

while (client.available()) {
  String line = client.readStringUntil('\n');
  StaticJsonDocument<256> jsonBuffer;
  DeserializationError error = deserializeJson(jsonBuffer, line);
  
  if (!error) {
    const char* value = jsonBuffer["value"];
    Serial.println(value);
  }
}
```

위의 코드는 간단한 HTML 파싱의 예제입니다. Arduino가 웹페이지에 연결하고, 페이지의 내용을 읽어들입니다. 그 다음, HTML 내용을 파싱하여 우리가 필요한 정보를 추출합니다.

## 깊은 탐색:

HTML 파싱은 웹 크롤링의 주요 요소여서, 인터넷 초기 단계부터 존재했습니다. XPATH, CSS Selector 등 다양한 파싱 방법이 있습니다. 

또한, 파싱 방식에는 Stream-based 파싱과 DOM-based 파싱이 있습니다. Stream 기반 파싱은 메모리 효율적입니다. 반면 DOM 기반 파싱은 콘텐츠를 트리 구조로 바꾸어 이해하기 쉽고 코딩하기 편합니다.

## 참고 링크:

1. [ArduinoJson 라이브러리](https://arduinojson.org/)