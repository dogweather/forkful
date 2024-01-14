---
title:                "Java: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증과 함께 보내는 것에 대해 생각해보면, 보안적인 이유로 인해 서버에 접근할 필요가 있는 경우 일반적으로 이와 같은 인증 방식을 사용합니다.

## 어떻게?

아래 예시 코드를 참고하여 기본 인증과 함께 HTTP 요청을 보내는 방법을 살펴보겠습니다.

```Java
// 필요한 라이브러리를 임포트합니다
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Base64;

// 서버 URL과 인증 정보를 설정합니다
String urlString = "https://www.example.com";
String username = "username";
String password = "password";

// URL 객체를 생성합니다
URL url = new URL(urlString);

// HTTP 연결을 설정합니다
HttpURLConnection connection = (HttpURLConnection) url.openConnection();

// 기본 인증 헤더를 설정합니다
String auth = username + ":" + password;
String basicAuth = "Basic " + Base64.getEncoder().encodeToString(auth.getBytes());
connection.setRequestProperty("Authorization", basicAuth);

// HTTP 요청을 보냅니다
connection.setRequestMethod("GET");

// 응답 코드를 확인합니다
int responseCode = connection.getResponseCode();

// 응답 본문을 읽어옵니다
BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
String inputLine;
StringBuffer response = new StringBuffer();
while ((inputLine = reader.readLine()) != null) {
   response.append(inputLine);
}
reader.close();

// 응답 코드와 본문을 확인합니다
System.out.println("응답 코드: " + responseCode);
System.out.println("응답 본문: " + response.toString());
```

### 결과

```
응답 코드: 200
응답 본문: <html><body><h1>Hello World!</h1></body></html>
```

## 더 깊게

HTTP 요청에는 여러 가지 인증 방식이 있지만, 기본 인증은 가장 간단하고 일반적인 방식입니다. 이 방식에서는 클라이언트가 보내는 인증 정보를 URL에서 인코딩하여 헤더에 포함시킵니다. 그리고 서버는 받은 인증 정보를 디코딩하여 인증을 확인하고, 요청에 대한 응답을 보냅니다. 하지만 이 방식은 보안성이 낮은 편이기 때문에, 더 나은 보안이 요구되는 경우에는 다른 인증 방식을 사용하는 것을 권장합니다.

## 참고 자료

- [Java URL 클래스](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [Java HttpURLConnection 클래스](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Base64 인코딩](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)