---
title:                "HTML 파싱"
date:                  2024-01-20T15:29:55.049999-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
HTML 파싱은 HTML 마크업에서 데이터를 추출하는 과정입니다. 프로그래머들은 웹페이지의 데이터를 자유롭게 활용하기 위해 이를 수행합니다.

## How to: (방법)
```Arduino
#include <Ethernet.h>
#include <HttpClient.h>

EthernetClient ethClient;
HttpClient client = HttpClient(ethClient, "example.com");
int statusCode = 0;
String response;

void setup() {
  Ethernet.begin(mac);
  Serial.begin(9600);
  client.get("/path/to/resource");
  statusCode = client.responseStatusCode();
  response = client.responseBody();
  parseHtml(response);
}

void parseHtml(String htmlContent) {
  // HTML 파싱 로직: 간단한 예시
  int startIndex = htmlContent.indexOf("<title>") + 7;
  int endIndex = htmlContent.indexOf("</title>");
  if (startIndex > 0 && endIndex > 0) {
    String pageTitle = htmlContent.substring(startIndex, endIndex);
    Serial.println(pageTitle);
  }
}

void loop() {
  // 여기서는 필요없습니다.
}
```
**Sample Output:**
```
Example Domain
```

## Deep Dive (심층 분석)
HTML 파싱은 1990년대 웹의 초창기부터 필요했습니다. 초기에 파싱은 주로 서버 사이드에서 이루어졌으나, IoT의 발달로 마이크로컨트롤러에서도 필요해졌습니다. Arduino 같은 보드에는, 리소스가 제한적이므로 경량 라이브러리 또는 단순한 문자열 처리를 주로 사용합니다. 대안으로는 regex(정규 표현식) 또는 전용 HTML 파싱 라이브러리가 있지만, 메모리 제한 떄문에 Arduino에서는 일반적이지 않습니다. 구현할 때는 HTML의 구조를 잘 이해하고 있어야 하며, 파싱 로직은 대상 HTML에 따라 달라질 수 있습니다.

## See Also (추가 정보)
- Arduino Ethernet 라이브러리: https://www.arduino.cc/en/Reference/Ethernet
- Arduino HttpClient 라이브러리: https://github.com/amcewen/HttpClient
- HTML 파싱에 대한 일반적인 정보: https://www.w3schools.com/html/html_intro.asp
- 문자열 처리에 대한 Arduino 참고자료: https://www.arduino.cc/reference/en/language/variables/data-types/string/
