---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 보내는 것은 웹 서버에게 정보를 요청하거나 보내는 방법입니다. 이는 대부분의 웹 기반 애플리케이션에서 핵심적인 기능이며, 데이터를 다루거나 원격 서비스와 상호 작용을 하는 등 다양한 시나리오에서 사용됩니다.

## 어떻게 쓰는가:

아래는 libcurl, 오픈 소스 URL 전송 라이블러리를 사용하여 HTTP 요청을 보내는 C 프로그램 예제입니다.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    /* Perform the request */
    res = curl_easy_perform(curl);

    /* Check for errors */
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    /* Cleanup */
    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```

이 프로그램을 실행하면 "http://example.com"에 HTTP 요청을 보냅니다. 요청 결과가 오류라면 오류 메시지를 출력합니다.

## 깊은 탐구:

- **역사적 맥락:** HTTP는 1991년에 팀 버너스-리에 의해 설계되었습니다. HTTP 요청은 기본적으로 클라이언트와 서버 간의 통신 방법을 정의하는 이 프로토콜의 핵심 요소입니다.
- **대체 방법:** HTTP 요청 외에도 SOAP, GraphQL 같은 다양한 프로토콜과 방식이 있습니다. 선택은 필요에 따라 달라집니다.
- **구현 세부 정보:** libcurl을 사용하여 HTTP 요청을 보내면, 라이브러리는 TCP/IP 연결을 관리하고 HTTP 프로토콜의 복잡한 세부 사항을 처리합니다.

## 참고 리소스:

- [Libcurl 홈페이지](https://curl.haxx.se/libcurl/)
- [HTTP - Wikipedia](https://ko.wikipedia.org/wiki/HTTP)
- [C 프로그래밍/C언어로 HTTP 요청 보내기](https://wikidocs.net/3549)