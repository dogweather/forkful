---
title:                "HTTP 요청 보내기"
html_title:           "Arduino: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

서울 환영합니다! 오늘은 아두이노로 HTTP 요청을 보내는 방법에 대해 알아보겠습니다. 아두이노를 사용하여 HTTP 요청을 보내는 것은 인터넷을 통해 다른 장치나 서비스와 통신하는 데 유용합니다. 자, 그래서 왜 HTTP 요청을 보내야 할까요?

## 왜?

아두이노로 HTTP 요청을 보내는 것은 다양한 서비스나 장치와 통신하기 위해서입니다. 예를 들어, 온도나 습도를 측정하는 센서를 이용하여 해당 데이터를 웹서버에 업로드하고 다른 장치에서 이를 활용할 수 있습니다. 또는 인터넷으로 제어 가능한 장치를 만들 때, 웹서버에서 HTTP 요청을 보내면 해당 장치를 제어할 수 있습니다. 아두이노로 HTTP 요청을 보내는 것은이와 같은 다양한 용도로 사용될 수 있습니다.

## 어떻게?

아두이노에서 HTTP 요청을 보내기 위해서는 Wi-Fi 모듈이나 이더넷 쉴드와 같은 인터넷 연결 모듈이 필요합니다. 이 모듈을 사용하여 인터넷에 연결한 후, 아래 코드를 이용하여 HTTP 요청을 보낼 수 있습니다.

```Arduino
#include <WiFi.h> // Wi-Fi 모듈 사용을 위한 라이브러리 불러오기

char ssid[] = "Wifi 이름"; // Wi-Fi 이름
char pass[] = "비밀번호"; // Wi-Fi 비밀번호
char server[] = "웹서버 주소"; // HTTP 요청을 보낼 웹서버 주소

void setup() {
    Serial.begin(9600); // 시리얼 모니터를 사용하기 위한 설정
    WiFi.begin(ssid, pass); // Wi-Fi 연결 시작
    while (WiFi.status() != WL_CONNECTED) { // Wi-Fi 연결이 완료될 때까지 대기
        delay(500);
        Serial.print(".");
    }
    Serial.println("Connected to WiFi!"); // Wi-Fi 연결이 완료되면 메시지 출력
}

void loop() {
    if (WiFi.status() == WL_CONNECTED) { // Wi-Fi 연결 상태를 확인
        HTTPClient http; // HTTP 요청을 보낼 수 있는 라이브러리 생성
        http.begin(server); // HTTP 요청을 보낼 웹서버 주소 설정
        int httpCode = http.GET(); // GET 방식으로 HTTP 요청 보내기
        
        if (httpCode > 0) { // HTTP 요청 성공 시
            String response = http.getString(); // 웹서버의 응답 받아오기
            Serial.println(httpCode); // HTTP 응답 코드 출력
            Serial.println(response); // 웹서버 응답 출력
        }
        else { // HTTP 요청 실패 시
            Serial.println("Error on HTTP request"); // 에러 메시지 출력
        }
        http.end(); // HTTP 요청 종료
    }
    else { // Wi-Fi 연결 실패 시
        Serial.println("Error on WiFi connection"); // 에러 메시지 출력
    }
    delay(60000); // 1분마다 HTTP 요청을 보내도록 대기
}
```

위 코드는 Wi-Fi 모듈을 사용하여 HTTP 요청을 보내는 예제입니다. `ssid`와 `pass` 변수에 자신의 Wi-Fi 정보를 입력하고, `server` 변수에는 HTTP 요청을 보낼 웹서버의 주소를 입력합니다. 이후 `http.begin()` 메소드를 사용하여 웹서버 주소를 설정하고, `http.GET()` 메소드를 사용하여 `GET` 방식으로 HTTP 요청을 보냅니다. 그리고 HTTP 응답을 받아와 읽어오기 위해 `http.getString()` 메소드를 사용합니다.