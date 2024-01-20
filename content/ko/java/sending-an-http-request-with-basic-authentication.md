---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 필요한가요? (What & Why?)
HTTP 요청에 기본 인증을 추가하는 것은 웹 서버로 전송되는 사용자의 정보를 인증하고 보호하기 위한 것입니다. 이를 통해 개발자는 사용자의 권한을 체크하고, 제대로된 사용자만 요청을 처리하도록 할 수 있습니다.

## 어떻게 동작하는지 (How to)
다음은 Java에서 HTTP 요청에 기본 인증을 추가하는 방법입니다. 

```Java
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;

public class Main {
    public static void main(String[] args) throws Exception {

        HttpClient client = HttpClient.newBuilder().authenticator(new Authenticator() {
            @Override
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication("username", "password".toCharArray());
            }
        }).build();

        HttpRequest request = HttpRequest.newBuilder()
                .uri(new URI("http://example.com"))
                .GET()
                .build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}

```
위 코드를 실행하면 "http://example.com" 사이트로 HTTP GET 요청이 전송되고, 그 응답이 콘솔에 출력될 것입니다.

## 깊은 이해를 위하여 (Deep Dive)
HTTP에 기본 인증을 추가하는 방법은 1990년대부터 사용되어 왔으며, 이것은 HTTP/1.0 표준의 일부입니다. 하지만 오늘날에는 보안성 문제로 인하여 대체적으로 다른 인증 방식들(예를 들어, OAuth 또는 JWT)로 바뀌고 있습니다.

Java에서는 `java.net.http.HttpClient` 클래스를 이용하여 HTTP 요청을 보낼 수 있으며, `HttpClient.newBuilder().authenticator`를 이용하여 기본 인증을 추가할 수 있습니다. `Authenticator` 객체는 요청을 보낼 때 호출되고, 요청에 부합하는 인증정보를 반환합니다.

## 참조 링크 (See Also)
- Java SE 11 & JDK 11: HTTP Client: [https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- HTTP basic authentication : [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)