---
title:                "Arduino: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청을 하기 위해서 기본 인증을 사용하는 이유는 인증된 사용자의 정보를 안전하게 전송하기 위해서입니다.

## 하기
HTTP 요청을 보낼 때 기본 인증을 사용하는 방법은 아래 코드와 같습니다. 이를 이용하여 Arduino와 연결된 장치로부터의 데이터를 안전하게 전송할 수 있습니다.

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

// 인터넷에 연결
const char* ssid = "와이파이 이름";
const char* password = "와이파이 비밀번호";

// 요청을 보낼 주소
const char* host = "서버 주소";
const int port = 80;

// 기본 인증을 위한 사용자 정보
const char* user = "사용자 이름";
const char* password = "사용자 비밀번호";

// WiFi 연결 함수
void connectWiFi() {
  Serial.print("WiFi 연결 시도 중...");
  
  WiFi.begin(ssid, password); // WiFi 연결
  
  while (WiFi.status() != WL_CONNECTED) { // 연결 상태 확인
    delay(500);
    Serial.print(".");
  }
  
  Serial.println(" 연결 완료!");
}

void setup() {
  Serial.begin(9600); // 시리얼 포트 설정
  connectWiFi(); // WiFi 연결
}

void loop() {
  HTTPClient http; // HTTPClient 객체 생성
  
  // 인증 정보 설정
  http.setAuthorization(user, password);

  // 서버에 GET 요청 보내기
  http.begin("http://server-address.com");
  int httpCode = http.GET(); // GET 요청 보내기
  
  if (httpCode > 0) {
    Serial.println(httpCode);
    String payload = http.getString(); // 서버로부터 받은 데이터 읽기
    Serial.println(payload); // 데이터 출력
  }
  
  http.end(); // HTTP 연결 해제
  delay(5000); // 5초 대기
}
```

위 코드는 서버와 연결하고, 인증 정보를 설정한 다음, GET 요청을 보내서 서버로부터 응답을 받아서 출력해주는 예제입니다. 여러분은 이를 기반으로 서버로부터 받은 데이터를 자유롭게 처리할 수 있습니다.

## 딥 다이브
HTTP 요청을 보낼 때 기본 인증을 사용하는 것은 서버로부터 데이터를 안전하게 전송할 수 있는 중요한 요소입니다. 기본 인증은 사용자의 이름과 비밀번호를 미리 설정해두고, 이를 클라이언트 측에서 서버로 전송해서 인증하는 방식입니다. 이를 통해 외부에서의 악의적인 접근을 방지할 수 있습니다.

## 관련 링크
- [WiFi 라이브러리](http://www.arduino.cc/en/Reference/WiFi)
- [HTTPClient 라이브러리](http://www.arduino.cc/en/Reference/HTTPClient)
- [HTTP 요청과 기본 인증](https://www.tutorialspoint.com/http/http_authentication.htm)