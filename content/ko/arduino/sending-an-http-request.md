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

## 무엇이고 왜?

HTTP 요청을 보내는 것은 인터넷을 통해 다른 컴퓨터나 서버에 데이터를 요청하는 것을 말합니다. 프로그래머들은 주로 웹 서비스나 API와 연동하기 위해서 이 작업을 수행합니다.

## 어떻게:

```
ArduinoClient client;
int port = 80;
if (client.connect("www.example.com", port)) {
  client.println("GET /index.html HTTP/1.1");
  client.println("Host: www.example.com");
  client.println();
}
while (client.available()) {
  char c = client.read();
  Serial.write(c);
}
```

위의 코드는 예시로, www.example.com 에서 index.html 파일을 요청하고, 해당 서버에서 보내는 응답을 아두이노로 받아서 출력합니다.

## 깊게 파고들기:

HTTP 요청은 현재 인터넷에서 가장 일반적으로 사용되는 프로토콜 중 하나입니다. 다른 대안으로는 FTP, SMTP 등이 있습니다. HTTP는 데이터를 요청하고 응답받는 과정을 정확하게 지정한 규약으로, 이를 따르는 서비스나 API는 다양한 시스템에서 연동 가능합니다.

## 관련 자료:

[HTTP 프로토콜 소개](https://www.w3schools.com/js/js_ajax_intro.asp)

[아두이노에서 HTTP 요청 보내기 예시](https://www.arduino.cc/en/Tutorial/HttpClient)

[HTTP와 다른 프로토콜 비교](https://medium.com/@rcdexta/the-difference-between-http-and-other-protocols-51cd0c13e071)