---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:36.295569-07:00
description: "\uBC29\uBC95: \uC544\uB450\uC774\uB178\uC5D0\uC11C HTML\uC744 \uD30C\
  \uC2F1\uD558\uB824\uBA74 \uC81C\uD55C\uB41C \uAE30\uAE30 \uC790\uC6D0 \uB54C\uBB38\
  \uC5D0 \uCD5C\uC18C\uD55C\uC758 \uBA54\uBAA8\uB9AC\uB97C \uCC28\uC9C0\uD558\uB294\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\uC694\uD569\uB2C8\uB2E4. \uC6F9 \uC2A4\
  \uD06C\uB798\uD551\uACFC \uD30C\uC2F1\uC744 \uC704\uD55C \uC778\uAE30 \uC788\uB294\
  \ \uC120\uD0DD\uC740 ESP8266 \uD639\uC740 ESP32\uC5D0\uC11C `ESP8266HTTPClient`\uC640\
  \ `ESP8266WiFi` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758\u2026"
lastmod: '2024-03-13T22:44:55.605708-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uB824\
  \uBA74 \uC81C\uD55C\uB41C \uAE30\uAE30 \uC790\uC6D0 \uB54C\uBB38\uC5D0 \uCD5C\uC18C\
  \uD55C\uC758 \uBA54\uBAA8\uB9AC\uB97C \uCC28\uC9C0\uD558\uB294 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uAC00 \uD544\uC694\uD569\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

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
