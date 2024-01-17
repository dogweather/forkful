---
title:                "HTTP 요청 보내기"
html_title:           "Java: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTTP 요청을 보내는 것은 원격 서버에 정보를 요청하고 응답을 받는 과정을 말합니다. 프로그래머들이 이를 하는 이유는 웹 애플리케이션을 개발하거나 데이터를 가져오는 등 다양한 목적을 위해서입니다.

## 하우 투:
```Java
// Java에서 HTTP 요청 보내기 예제
URL url = new URL("http://www.example.com/"); // 요청을 보낼 URL 생성
HttpURLConnection con = (HttpURLConnection) url.openConnection(); // URL을 통해 연결 생성
con.setRequestMethod("GET"); // GET 요청 설정
int responseCode = con.getResponseCode(); // 응답 코드 받기
System.out.println("응답 코드: " + responseCode); // 200 OK가 출력됩니다.
```

## 딥 다이브:
HTTP 요청은 웹 서버와의 통신에 필수적인 기술입니다. HTTP 프로토콜은 1990년대 초에 Tim Berners-Lee에 의해 개발되었으며, 웹 애플리케이션 개발에 큰 영향을 미쳤습니다. 이제는 서버와의 통신에 다른 프로토콜을 사용하는 경우도 있지만, 여전히 HTTP 요청은 널리 사용되고 있습니다. HTTP 요청을 보낼 때는 GET, POST 등의 메소드를 선택할 수 있으며, URL의 파라미터 또는 요청 본문에 데이터를 넣어 서버에 전송할 수 있습니다.

## 관련 자료:
- [Java에서 HTTP 요청 보내기](https://www.baeldung.com/java-http-request)
- [HTTP 프로토콜 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Overview)