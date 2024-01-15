---
title:                "html 파싱"
html_title:           "Arduino: html 파싱"
simple_title:         "html 파싱"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## 왜 HTML 파싱을 하는가?

웹 개발을 하게 되면 HTML 코드를 다루는 일이 빈번하게 발생합니다. 따라서 HTML 코드를 분석하고 원하는 데이터를 추출하는 방법을 배우는 것은 매우 중요합니다.

## 어떻게 하면 될까요?

여기에서는 Arduino를 사용하여 HTML 코드를 파싱하는 예제를 제공하겠습니다.

```
Arduino 코드 예제:
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

void setup() {
    Serial.begin(115200); // 시리얼 통신 시작
    WiFi.begin("네트워크 이름", "비밀번호"); // WiFi 연결
    while (WiFi.status() != WL_CONNECTED) { // 연결될 때까지 대기
        delay(500);
        Serial.print(".");
    }
    Serial.println("WiFi 연결 완료!");

    // HTTP 클라이언트 생성
    HTTPClient http;

    // 요청 보내기
    http.begin("https://example.com");  // 파싱할 사이트의 URL
    int httpCode = http.GET();  // GET 요청 보내기
    if (httpCode > 0) {
        String html = http.getString();  // 응답 받은 HTML 코드 저장
        int startTag = html.indexOf("원하는 데이터의 시작 태그");  // 해당 문자열의 시작 위치 찾기
        int endTag = html.indexOf("원하는 데이터의 끝 태그", startTag);  // 해당 문자열의 끝 위치 찾기
        String data = html.substring(startTag, endTag);  // 원하는 데이터 추출
        Serial.println(data);  // 시리얼 모니터에 출력
    }
    http.end();  // 연결 끊기
}

void loop() {
    // 할 일 없음
}
```

위 코드를 실행하면 파싱한 데이터를 시리얼 모니터에 보여줍니다.

## 더 깊게 알아보기

HTML 파싱은 더 복잡한 웹 크롤러 혹은 스크래퍼를 만들 때 매우 유용합니다. 또한, 데이터 분석 및 가공에도 활용할 수 있습니다. 따라서 HTML 파싱에 대한 깊은 이해는 개발에 있어서 매우 중요합니다.

## 관련 자료

- [ESP8266WiFi 라이브러리 문서](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [HTML 파싱 라이브러리 참고용 코드](https://gist.github.com/dustinblackman/ff08b0b3593d3d71f8ce9030aba718bf)
- [웹 크롤링과 스크래핑에 대한 더 자세한 정보](https://developer.mozilla.org/ko/docs/Learn/Tools_and_testing/Cross_browser_testing/HTML_parser)