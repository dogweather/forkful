---
title:                "Java: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것이 왜 중요한지 궁금하신가요? HTTP는 웹 브라우저와 웹 서버 사이에서 데이터를 주고받는 데 사용되는 프로토콜입니다. 따라서 HTTP 요청은 웹 페이지를 로드하거나 온라인 정보를 검색하는 데 필수적입니다.

## 어떻게

Java에서 HTTP 요청을 보내는 방법은 간단합니다. 먼저 `HttpURLConnection` 클래스를 사용하여 웹 서버에 연결하고, `getRequestMethod()` 메소드를 사용하여 요청 메소드를 설정해야 합니다. 그리고 요청 본문에 데이터를 추가하고 요청을 보냅니다. 아래는 GET 요청 예제 코드입니다.

```java
try {
    // 요청을 보낼 URL 생성
    URL url = new URL("https://www.example.com/api/users");

    // HttpURLConnection 객체 생성
    HttpURLConnection con = (HttpURLConnection) url.openConnection();

    // 요청 메소드 설정
    con.setRequestMethod("GET");

    // 응답 코드 확인
    int responseCode = con.getResponseCode();
    System.out.println("Response code: " + responseCode);

    // 응답 본문 읽기
    BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
    String inputLine;
    StringBuffer response = new StringBuffer();
    while ((inputLine = in.readLine()) != null) {
        response.append(inputLine);
    }
    in.close();

    // 응답 출력
    System.out.println(response.toString());
} catch (IOException e) {
    e.printStackTrace();
}
```

위 코드를 실행하면 `/api/users` 경로로 GET 요청을 보내고, 서버에서 응답을 받아 출력하게 됩니다. 예를 들어, 응답 코드가 200이고 응답 본문에 사용자 목록 데이터가 포함되어 있다면 다음과 같은 결과를 볼 수 있습니다.

```json
[
    {"id": 1, "name": "John", "age": 30},
    {"id": 2, "name": "Jane", "age": 25},
    {"id": 3, "name": "Bob", "age": 40}
]
```

## 딥 다이브

실제로 HTTP 요청을 보내기 전에는 DNS 조회, TCP 연결 설정, SSL 암호화, 요청과 응답 헤더 등의 다양한 과정이 이뤄집니다. 또한 요청 메소드를 선택하는 것도 중요합니다.

가장 일반적인 요청 메소드는 GET, POST, PUT, DELETE입니다. 각각의 역할은 아래와 같습니다.

- GET: 서버에서 데이터를 요청하는 메소드로, 보통 브라우저에서 링크를 클릭하거나 URL을 입력할 때 사용됩니다.
- POST: 서버로 데이터를 전송하는 메소드로, 로그인 폼처럼 데이터를 입력하고 제출할 때 주로 사용됩니다.
- PUT: 서버에서 데이터를 수정하는 메소드로, REST API에서 자주 사용됩니다.
- DELETE: 서버에서 데이터를 삭제하는 메소드로, 일반적으로 사용자가 삭제 버튼을 클릭할 때 사용됩니다.

HTTP 요청을 더 자세히 알고 싶다면 [여기](https://developer.mozilla.org/ko/docs/Web/HTTP/Messages)를 참고해주세요.

## 참고 자료

- [Java에서 HTTP 요청 보내는 방법](https://www.baeldung.com/java-http-request)
- [HTTP 메소드에 대한 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Methods)
- [HTTP 요청과 응답의 구조](https://developer.mozilla.org/ko/docs/Web/HTTP/Messages)