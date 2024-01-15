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

## 왜

HTTP 요청을 보내는 것에 대해 고민하고 있는 이유는 무엇일까요? 만약 데이터를 서버로 보내고 받거나 웹 페이지를 불러오는 등 인터넷을 이용하는 일이 있다면, 당신은 이미 HTTP 요청을 사용하고 있습니다. 이번 기사에서는 C 언어로 HTTP 요청을 보내는 방법을 알려드리겠습니다.

## 어떻게

우선, HTTP 요청을 보내는 데 필요한 라이브러리를 헤더 파일에 포함해야 합니다. `stdio.h`, `stdlib.h`, `string.h`, `curl/curl.h`와 같은 헤더 파일을 사용합니다. 아래의 코드를 참고해보세요.

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>
```

다음으로, `main()` 함수 내에서 HTTP 요청을 보낼 URL과 메소드를 설정해주어야 합니다. 아래의 예시 코드는 `example.com` 서버에 `GET` 메소드로 요청을 보내는 예시입니다.

```C
int main() {
  // 요청을 보낼 URL
  char *url = "https://example.com";

  // 요청의 메소드
  char *method = "GET";

  // HTTP 요청 보내기
  CURL *curl;
  CURLcode res;
  curl = curl_easy_init();
  if (curl) {
    // URL 설정
    curl_easy_setopt(curl, CURLOPT_URL, url);
    // 메소드 설정
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, method);
    // 요청 보내기
    res = curl_easy_perform(curl);
    // 오류 처리
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    // curl 핸들 해제
    curl_easy_cleanup(curl);
  }

  return 0;
}
```

위의 코드를 실행하면, `example.com` 서버로부터 받은 응답 메시지를 콘솔에 출력합니다.

## 깊게 파기

HTTP 요청을 보내는 프로그램에서 가장 중요한 부분은, `CURLOPT_URL`과 `CURLOPT_CUSTOMREQUEST` 옵션이며, 이 둘을 통해 URL과 메소드를 설정합니다. 그 외에도 `CURLOPT_HTTPHEADER` 옵션을 사용하여 Header 설정이나 `CURLOPT_POSTFIELDS` 옵션으로 Body를 함께 보낼 수 있습니다. 더 자세한 내용은 `libcurl`의 문서를 참고하시기 바랍니다.

## 참고

- [libcurl 공식 문서](https://curl.haxx.se/libcurl/c/)
- [HTTP 요청 보내기 예제 코드](https://curl.haxx.se/libcurl/c/simple.html)