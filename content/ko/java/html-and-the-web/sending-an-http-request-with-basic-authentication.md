---
date: 2024-01-20 18:02:13.971213-07:00
description: "HTTP \uC694\uCCAD\uC5D0 \uAE30\uBCF8 \uC778\uC99D\uC744 \uCD94\uAC00\
  \uD558\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\
  \uD638\uB85C \uBCF4\uD638\uB41C \uB9AC\uC18C\uC2A4\uC5D0 \uC811\uADFC\uD560 \uB54C\
  \ \uD544\uC694\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBCF4\
  \uC548\uC774 \uD544\uC694\uD55C \uB370\uC774\uD130\uB97C \uC548\uC804\uD558\uAC8C\
  \ \uC804\uC1A1\uD558\uACE0 \uC778\uC99D\uB41C \uC0AC\uC6A9\uC790\uB9CC \uC815\uBCF4\
  \uB97C \uBC1B\uC744 \uC218 \uC788\uAC8C \uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uC220\
  \uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.048009-06:00'
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD\uC5D0 \uAE30\uBCF8 \uC778\uC99D\uC744 \uCD94\uAC00\uD558\
  \uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\
  \uB85C \uBCF4\uD638\uB41C \uB9AC\uC18C\uC2A4\uC5D0 \uC811\uADFC\uD560 \uB54C \uD544\
  \uC694\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBCF4\uC548\
  \uC774 \uD544\uC694\uD55C \uB370\uC774\uD130\uB97C \uC548\uC804\uD558\uAC8C \uC804\
  \uC1A1\uD558\uACE0 \uC778\uC99D\uB41C \uC0AC\uC6A9\uC790\uB9CC \uC815\uBCF4\uB97C\
  \ \uBC1B\uC744 \uC218 \uC788\uAC8C \uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uC220\
  \uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청에 기본 인증을 추가하는 것은 사용자 이름과 비밀번호로 보호된 리소스에 접근할 때 필요합니다. 프로그래머들은 보안이 필요한 데이터를 안전하게 전송하고 인증된 사용자만 정보를 받을 수 있게 하기 위해 이 기술을 사용합니다.

## How to: (어떻게 하나요?)
Java에서 실행 가능한 기본 인증을 포함한 HTTP 요청을 보내는 예제입니다. 단순함에 집중합니다.

```java
import java.net.URL;
import java.net.HttpURLConnection;

public class HttpBasicAuth {
    
    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com/api/data");
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();

            // 인코딩된 사용자 이름과 비밀번호
            String encodedCredentials = Base64.getEncoder().encodeToString("username:password".getBytes());
            
            // 기본 인증 추가
            connection.setRequestProperty("Authorization", "Basic " + encodedCredentials);

            // 요청을 얻고 출력합니다
            connection.setRequestMethod("GET");
            System.out.println("Response Code: " + connection.getResponseCode());
            System.out.println("Response Message: " + connection.getResponseMessage());
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

실행 결과:

```
Response Code: 200
Response Message: OK
```

## Deep Dive (심긴 탐험)
HTTP 기본 인증은 RFC 7617에 정의된 가장 간단한 인증 메커니즘입니다. `username:password` 형식을 Base64로 인코딩하여 `Authorization` 헤더에 추가합니다. 이 방식은 HTTPS와 함께 사용할 때 더 안전합니다.

대안으로, OAuth 같은 보다 복잡한 인증 방식이 있습니다. OAuth는 토큰 기반이며 권한 부여를 통해 더 세밀한 접근 제어를 허용합니다.

Java 11부터 `java.net.http` 패키지가 소개되었고, `HttpClient`, `HttpRequest`, `HttpResponse` 클래스를 이용해 HTTP 요청을 더 쉽게 처리할 수 있습니다. 이는 `HttpURLConnection`에 비해 더 modern하고 functional한 API를 제공합니다.

## See Also (더 보기)
- [Java HttpURLConnection Documentation](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Base64 인코딩/디코딩 JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Java 11 HttpClient Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
