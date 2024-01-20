---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
기본 인증이 있는 HTTP 요청을 보내는 것은, 웹 서버에 정보를 요청하거나 전달하는 방법입니다. 프로그래머들은 이것을 데이터 교환을 안전하게 수행하려고 사용합니다.

## 어떻게 사용하는가:
C 언어로 기본 인증을 가진 HTTP 요청을 보내는 간단한 코드는 아래와 같습니다:

```C
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "user:password");

    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
       fprintf(stderr, "curl_easy_perform() failed: %s\n",
               curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  curl_global_cleanup();
  return 0;
}
```
해당 코드를 실행하면, 당신은 http://example.com에 기본 인증 (사용자 이름: user, 비밀번호: password)로 HTTP 요청을 보낼 수 있습니다.

## Deep Dive
기본 인증이 있는 HTTP 요청은 1990년대 초반에 고안되었고 HTTP/1.0 이후로 널리 사용되었습니다. **libcurl** 라이브러리는 네트워크 전송을 처리하기 위해 널리 사용되는 라이브러리 중 하나이며, 기본 인증을 제공합니다. 비록 더 안전한 대안들이 개발되었음에도 불구하고, 기본 인증은 허용된 경우 매우 간단하게 구현할 수 있다는 점에서 장점을 가지고 있습니다.

## See Also
- libcurl 공식 문서: https://curl.haxx.se/libcurl/
- 기본 인증에 대해 더 자세히 알아보기: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication