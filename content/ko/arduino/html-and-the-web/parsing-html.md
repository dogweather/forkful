---
title:                "HTML 파싱"
aliases:
- /ko/arduino/parsing-html.md
date:                  2024-02-03T19:11:36.295569-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜?

아두이노 프로젝트에서 HTML 파싱은 웹 페이지로부터 정보를 추출하는 것을 말합니다. 프로그래머들은 이를 통해 아두이노 기기가 인터넷과 상호작용하도록 하여, 홈 자동화부터 환경 모니터링에 이르기까지 다양한 목적을 위해 웹사이트에서 데이터를 수집하게 합니다.

## 방법:

아두이노에서 HTML을 파싱하려면 제한된 기기 자원 때문에 최소한의 메모리를 차지하는 라이브러리가 필요합니다. 웹 스크래핑과 파싱을 위한 인기 있는 선택은 ESP8266 혹은 ESP32에서 `ESP8266HTTPClient`와 `ESP8266WiFi` 라이브러리의 사용입니다, 왜냐하면 이들은 Wi-Fi 기능과 HTTP 프로토콜을 위한 네이티브 지원을 제공하기 때문입니다. 여기 ESP8266이나 ESP32와 작업한다고 가정했을 때, HTML을 가져오고 파싱하는 기본 예제를 보여드리겠습니다:

먼저 필요한 라이브러리를 포함합니다:
```cpp
#include <ESP8266WiFi.h> // ESP8266을 위해
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// ESP32를 사용한다면 유사한 ESP32 라이브러리를 사용하세요

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

Wi-Fi 네트워크에 연결합니다:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("연결 중...");
    }
}
```

HTTP 요청을 보내고 간단한 HTML 조각을 파싱합니다:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //Wi-Fi 연결 상태 확인
        HTTPClient http;  //HTTPClient 클래스의 객체 선언

        http.begin("http://example.com");  //요청 목적지 지정
        int httpCode = http.GET();  //요청 보내기

        if (httpCode > 0) { //반환 코드 확인
            String payload = http.getString();   //요청 응답 페이로드 가져오기
            Serial.println(payload);             //응답 페이로드 출력

            // 특정 부분 파싱, 예: 페이로드에서 title 추출
            int titleStart = payload.indexOf("<title>") + 7; // "<title>" 태그 이후로 이동하기 위해 +7
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("페이지 제목: ");
            Serial.println(pageTitle);
        }

        http.end();   //연결 종료
    }

    delay(10000); //10초마다 요청하기
}
```

샘플 출력 (http://example.com이 간단한 HTML 구조를 가지고 있다고 가정):
```
연결 중...
...
페이지 제목: Example Domain
```

이 예제는 HTML 페이지를 가져와서 `<title>` 태그 내용을 추출하는 방법을 보여줍니다. 보다 복잡한 HTML 파싱을 위해서는, 메모리 제약 때문에 조심스럽게 정규 표현식을 사용하거나, HTML 구조를 통해 탐색하기 위한 문자열 조작 함수를 고려하세요. 고급 파싱은 아두이노 표준 환경에 내장된 HTML 파싱 라이브러리가 포함되어 있지 않기 때문에, 다루고 있는 HTML의 특정 구조에 맞춰진 맞춤형 파싱 알고리즘을 포함하여 더 복잡한 접근이 필요할 수 있습니다.
