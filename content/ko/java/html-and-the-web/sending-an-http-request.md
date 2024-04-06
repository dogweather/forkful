---
date: 2024-01-20 17:59:56.930772-07:00
description: "How to: (\uBC29\uBC95) Java\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\
  \uB0B4\uB294 \uAC04\uB2E8\uD55C \uC608\uC81C\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.814011-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Java\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294\
  \ \uAC04\uB2E8\uD55C \uC608\uC81C\uC785\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (방법)
Java에서 HTTP 요청을 보내는 간단한 예제입니다.

```java
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class HttpRequestExample {
    public static void main(String[] args) throws IOException {
        String urlToRequest = "http://example.com";
        URL url = new URL(urlToRequest);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        
        try {
            connection.setRequestMethod("GET");
            
            int responseCode = connection.getResponseCode();
            System.out.println("Response Code: " + responseCode);
            
            if (responseCode == HttpURLConnection.HTTP_OK) {
                BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                String inputLine;
                StringBuilder response = new StringBuilder();
                
                while ((inputLine = in.readLine()) != null) {
                    response.append(inputLine);
                }
                in.close();
                
                System.out.println("Response: " + response.toString());
            } else {
                System.out.println("GET request not worked");
            }
        } finally {
            connection.disconnect();
        }
    }
}
```
실행 결과:
```
Response Code: 200
Response: ... (실제 응답 내용)
```

## Deep Dive (심층 분석)
HTTP 요청을 보내는 것은 웹의 초창기부터 이루어져 왔습니다. 초기의 인터넷은 정보 교환을 위해 설계되었으며, HTTP는 이를 위한 프로토콜로 자리잡았습니다. Java에서는 처음에 low-level의 `HttpURLConnection` 클래스를 사용하여 요청을 처리했으나, Java 11부터는 새로운 HTTP Client API가 도입되어 비동기 처리같은 현대적인 요구사항을 더 쉽게 처리할 수 있게 되었습니다.

다른 많은 언어와 프레임워크가 간편한 HTTP 통신을 위한 여러 라이브러리와 도구를 제공하지만, Java 역시 Apache HttpClient, OkHttp, Retrofit 등 다양한 도구를 갖추고 있습니다. 각각의 도구들은 성능, 사용 편의성, 추가 기능 측면에서 각기 장단점을 가지고 있으며 작업에 맞게 선택할 수 있습니다.

## See Also (참고 자료)
- [HTTPURLConnection (Oracle Official Documentation)](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [HttpClient (Oracle Official Documentation for Java 11+)](https://openjdk.java.net/groups/net/httpclient/intro.html)
- [OkHttp](https://square.github.io/okhttp/)
- [Retrofit](https://square.github.io/retrofit/)
