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

# 왜?

웹 페이지를 다운로드하려는 이유는 여러가지가 있을 수 있습니다. 예를 들어, 미리 다운로드한 데이터를 사용해 프로젝트를 실행할 수 있거나, 인터넷 연결이 불안정한 환경에서 웹 페이지를 읽을 수 있기 때문입니다.

## 방법

```Arduino
#include <WiFi.h> // WiFi 라이브러리 임포트
#include <HTTPClient.h> // HTTPClient 라이브러리 임포트

void setup() {
  Serial.begin(9600); // 시리얼 통신 속도 설정
  WiFi.begin("SSID", "password"); // WiFi 연결 설정
  while (WiFi.status() != WL_CONNECTED) { // WiFi 연결 대기
    delay(1000);
    Serial.println("Connecting to WiFi..");
  }
  Serial.println("Connected to WiFi!");
  
  HTTPClient http; // HTTPClient 객체 생성
  http.begin("https://www.example.com"); // HTTP 요청 주소 설정
  int httpCode = http.GET(); // GET 요청
  if (httpCode > 0) { // 요청 성공 시
    String payload = http.getString(); // 응답 데이터 저장
    Serial.println(payload); // 시리얼 모니터에 출력
  }
  http.end(); // HTTP 연결 종료
}

void loop() {
  // 웹 페이지 다운로드 및 출력이 한번만 실행되도록 설정
  // 반복문에서 실행하면 웹 페이지가 계속 다운로드되고 시리얼 모니터가 너무 많은 텍스트를 출력하기 때문입니다.
}
```

위 코드는 WiFi 라이브러리와 HTTPClient 라이브러리를 이용해 웹 페이지를 다운로드하는 방법을 보여줍니다. 먼저 WiFi.begin() 함수를 사용해 WiFi 연결을 설정하고, HTTPClient 객체를 생성한 뒤 http.begin() 함수를 사용해 웹 페이지의 주소를 설정합니다. 그리고 http.GET() 함수로 GET 요청을 보내고, 응답 코드를 확인하여 요청이 성공했을 때만 http.getString() 함수를 사용해 데이터를 받아옵니다. 마지막으로 http.end() 함수를 호출해 HTTP 연결을 종료합니다.

## 딥 다이브

웹 페이지를 다운로드하는 방법은 다양한데, 위 코드처럼 HTTP GET 요청을 보내는 방법 이외에도 다른 프로토콜을 사용할 수 있습니다. 또한, 웹 서버에서 보내는 응답 데이터를 파싱해 원하는 정보를 추출하는 기술도 필요합니다. 따라서 웹 페이지 다운로드와 관련된 기술을 자세히 학습하고 응용할 수 있는 능력을 갖추는 것이 중요합니다.

# 관련 자료

- [ESP32 WiFi 라이브러리 문서](https://www.arduino.cc/en/Reference/WiFi)
- [ESP32 HTTPClient 라이브러리 문서](https://github.com/espressif/arduino-esp32/tree/master/libraries/HTTPClient)
- [온라인 강좌 - 아두이노와 인터넷 연결하기](https://www.inflearn.com/course/아두이노-인터넷-연결)