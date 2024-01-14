---
title:                "C: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것에는 여러 가지 이유가 있습니다. 가장 일반적인 예는 사용자가 웹 사이트를 탐색할 때 웹 서버로부터 데이터를 요청하는 것입니다. 또는 API를 사용하여 다른 서비스에서 데이터를 가져오는 경우에도 HTTP 요청을 사용할 수 있습니다.

## 하는 방법

HTTP 요청을 보내는 것은 프로그래밍에서 꽤 흔한 작업입니다. 간단한 예제로 시작해보겠습니다. 먼저, 해당 작업에 필요한 라이브러리를 임포트해야 합니다. 그리고 요청을 보낼 URL, 메서드, 헤더 및 메시지 바디를 포함하여 요청에 필요한 모든 정보를 설정해야 합니다.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  CURLcode res;

  // 요청 보낼 URL 설정
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
    
    // 요청 메서드 설정
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "GET");
    
    // 헤더 설정
    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Accept: application/json");
    headers = curl_slist_append(headers, "Content-Type: application/json");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    
    // 메시지 바디 설정
    char *body = "{ \"message\": \"Hello World\" }"
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);

    // 요청 보내기
    res = curl_easy_perform(curl);
    
    // 결과 출력하기
    if(res == CURLE_OK) {
      printf("Request successful.");
    }
    
    // 라이브러리 정리하기
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

위의 예제는 cURL 라이브러리를 사용하여 HTTP 요청을 보내는 코드입니다. 이 예제 코드를 실행하면 `Request successful.`이라는 메시지가 출력됩니다. 이처럼 프로그래밍에서 HTTP 요청을 보내는 것은 매우 간단한 작업입니다.

## 깊이 들어가기

HTTP 요청을 보내는 것은 실제로 어떻게 동작할까요? HTTP 요청은 네트워크를 통해 데이터를 전송하는 것입니다. 프로그래밍에서는 일반적으로 TCP 소켓을 사용하여 네트워크 통신을 합니다. HTTP는 TCP를 기반으로 동작하는 프로토콜이기 때문에 이를 사용하여 HTTP 요청을 보낼 수 있습니다.

HTTP 요청은 주로 ASCII로 이루어진 텍스트 형식으로 전송됩니다. 예를 들어 위의 예제에서는 `https://example.com`이라는 URL을 설정했는데, 이 URL을 HTTP 요청으로 변환하면 다음과 같습니다.

```
GET / HTTP/1.1
Host: example.com
Accept: application/json
Content-Type: application/json
Content-Length: 23

{ "message": "Hello World" }
```

위의 예제에서 설정한 요청 메서드는 `GET`이며, `https://example.com`이라는 주소의 루트 페이지를 요청합니다. 그리고 `Accept` 및 `Content-Type` 헤더를 설정하고, 메시지 바디에 `Hello World`라는 메시지를 포함하여 요청을 보냅니다.

HTTP 요청의 예제 코드와 동작 방식을 살펴보았는데, 이제는 실제로 프로그래밍에서 HTTP 요청을 어떻게 사용할 수 있는지에 대해 더 많은 정보를 알