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

## 무엇 & 왜?

웹 페이지 다운로드는 인터넷에서 컴퓨터로 파일을 가져오는 것을 말합니다. 프로그래머는 웹 페이지를 다운로드해서 필요한 정보를 얻을 수 있기 때문에 이 작업을 합니다.

## 하는 법:

아래의 코드를 따라해보세요. 이 코드는 웹 페이지에서 "Hello World!"를 다운로드하고 콘솔에 출력합니다.

```Arduino

#include <WiFi.h>                // WiFi 라이브러리 불러오기
#include <HTTPClient.h>          // HTTP 클라이언트 라이브러리 불러오기

void setup() {

  // WiFi 연결하기
  WiFi.begin("WiFi 이름", "비밀번호");

  while (WiFi.status() != WL_CONNECTED) {     // 연결될 때까지 기다립니다.
    delay(500);
  }
}

void loop() {

  HTTPClient http;                      // HTTP 클라이언트 객체 생성하기

  http.begin("http://www.example.com");    // 다운로드할 웹 페이지 주소 설정하기
  int httpCode = http.GET();                 // HTTP GET 요청 보내기

  if (httpCode > 0) { // 정상적으로 응답이 온 경우
    String payload = http.getString();   // 응답받은 내용을 문자열로 저장하기
    Serial.println(payload);             // 콘솔에 출력하기
  }

  http.end(); // HTTP 클라이언트 객체 제거하기

  delay(5000); // 5초 기다리기
}

```

콘솔에는 "Hello World!"가 출력될 것입니다.

## 더 알아보기:

- 1990년대 초반까지는 FTP 프로토콜을 사용해서 파일을 다운로드했습니다. 그 이후에는 HTTP 프로토콜이 더 널리 사용되기 시작했습니다.

- 다른 방법으로는 curl 라이브러리나 urllib 라이브러리를 사용해서 웹 페이지를 다운로드할 수 있습니다.

- HTTP 프로토콜에서는 GET, POST, PUT, DELETE 등의 메서드를 사용해서 데이터를 다룰 수 있습니다.

## 관련 링크:

- [WiFi 라이브러리](https://www.arduino.cc/en/Reference/WiFi)
- [HTTPClient 라이브러리](https://github.com/arduino-libraries/HTTPClient)