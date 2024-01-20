---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

# HTTP 요청을 보내는 것은 무엇이며 왜 필요한가?

HTTP 요청은 웹서버에 정보를 요청하거나 전달하는 방법입니다. 개발자들은 이것을 통해 웹에 있는 다양한 자원들과 상호작용하게 됩니다.

# 어떻게 해야 할까?

Java에서 HTTP 요청을 보내기 위해 우리는 `java.net.http.HttpClient` 라이브러리를 사용한다. 아래에 예시를 나타내줍니다.

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
              .uri(new URI("http://example.com"))
              .build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}
```

위 코드를 실행하면 "http://example.com" 의 결과를 출력하게 됩니다.

# 더 깊게 알아보기

HTTP 요청은 웹의 기본적인 구조를 이루는 요소 중 한 가지입니다. 웹은 1990년대 중반에 처음 등장하여, 그 시작부터 HTTP는 핵심 역할을 하고 있습니다.

Java에서는 여러 가지 방법으로 HTTP 요청을 보낼 수 있습니다. 예를들어 `HttpURLConnection`, `HttpClient` 등이 있습니다. 이 중 `HttpClient`는 Java 11에서 추가되었으며, 편리한 API와 함께 모던한 기능들을 제공합니다.

내부적으로 `HttpClient` 는 각 요청을 병렬로 처리하며, 결과는 `CompletableFuture`를 통해 반환됩니다. 이러한 방식을 사용함으로써 Java는 효율적이고 확장가능한 HTTP 요청 처리를 제공합니다.

# 참고 자료

다음의 링크들에서 HTTP 요청과 관련된 더 많은 정보를 찾아볼 수 있습니다.

1. [Oracle의 HttpClient 문서](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
2. [모던 Java에서의 HTTP 통신](https://www.baeldung.com/httpclient-guide)