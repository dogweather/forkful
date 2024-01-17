---
title:                "HTTP 요청 보내기"
html_title:           "C: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 보내는 것이란 무엇일까요? 개발자들은 왜 이렇게 하느냐구요? HTTP 요청이란 간단히 말해서 서버에 데이터를 요청하는 것을 의미합니다. 예를 들어, 웹 페이지를 열 때마다 브라우저는 해당 웹 서버에 HTTP 요청을 보내고, 서버는 요청을 받아 해당하는 내용을 브라우저로 다시 보냅니다. 개발자들은 이것을 구현하는 이유는, 웹 서비스를 만들기 위해서이죠.

## 어떻게:

```c
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    /* always cleanup */
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

위 코드는 C 프로그래밍을 이용해 간단하게 HTTP 요청을 보내는 예제입니다. 이 프로그램을 컴파일하고 실행하면, www.example.com에서 가져온 웹 페이지를 볼 수 있습니다.

## 딥 다이브:

HTTP 요청은 인터넷에서 가장 중요한 컴퓨터 프로토콜 중 하나입니다. 이 프로토콜은 1989년 팀 버너스 리와 로이 필딩에 의해 시작되었으며, 현재까지도 계속 발전해오고 있습니다. HTTP 요청을 보내는 다른 방법으로는, cURL 이외의 다른 라이브러리를 사용하는 것도 있습니다. 또한 프로그램에서 직접 소켓을 이용해 HTTP 요청을 보낼 수도 있습니다. 하지만, cURL은 가장 널리 사용되는 방법 중 하나입니다. cURL은 다양한 운영체제에서 사용 가능하며, 간단하고 명확한 인터페이스를 제공합니다.

## 관련 자료:

- [libcurl 사용법](https://curl.se/libcurl/c/)
- [HTTP Request 관련 자세한 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Overview)