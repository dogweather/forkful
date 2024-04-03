---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:36.295569-07:00
description: "\uC544\uB450\uC774\uB178 \uD504\uB85C\uC81D\uD2B8\uC5D0\uC11C HTML \uD30C\
  \uC2F1\uC740 \uC6F9 \uD398\uC774\uC9C0\uB85C\uBD80\uD130 \uC815\uBCF4\uB97C \uCD94\
  \uCD9C\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uC544\uB450\uC774\uB178 \uAE30\uAE30\
  \uAC00 \uC778\uD130\uB137\uACFC \uC0C1\uD638\uC791\uC6A9\uD558\uB3C4\uB85D \uD558\
  \uC5EC, \uD648 \uC790\uB3D9\uD654\uBD80\uD130 \uD658\uACBD \uBAA8\uB2C8\uD130\uB9C1\
  \uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0 \uB2E4\uC591\uD55C \uBAA9\uC801\uC744 \uC704\
  \uD574 \uC6F9\uC0AC\uC774\uD2B8\uC5D0\uC11C \uB370\uC774\uD130\uB97C \uC218\uC9D1\
  \uD558\uAC8C \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.605708-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178 \uD504\uB85C\uC81D\uD2B8\uC5D0\uC11C HTML \uD30C\
  \uC2F1\uC740 \uC6F9 \uD398\uC774\uC9C0\uB85C\uBD80\uD130 \uC815\uBCF4\uB97C \uCD94\
  \uCD9C\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4."
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
