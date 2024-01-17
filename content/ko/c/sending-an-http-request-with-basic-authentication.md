---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "C: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTTP 요청에 기본 인증을 함께 보내는 것은 무엇일까요? 이것은 서버에서 인증 정보를 검증하고 사용자가 인증되었는지 확인하는 방법입니다. 프로그래머들이 이것을 하는 이유는 전송하는 데이터의 보안을 보장하고 인증이 필요한 서버에 접근하기 위해서입니다.

## 어떻게:
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
  
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
    res = curl_easy_perform(curl);
    
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    
    curl_easy_cleanup(curl);
  }  
  return 0;
}
```
위의 예시 코드는 libcurl 라이브러리를 사용하여 기본 인증을 추가한 HTTP 요청을 보내는 방법입니다. 다른 옵션들을 추가하면 요청을 더욱 세부적으로 컨트롤할 수 있습니다.

## 딥 다이브:
HTTP 기본 인증은 보안 및 인증이 필요한 서버에 접속하기 위해 사용되는 가장 오래된 형식의 사용자 인증 기술 중 하나입니다. 그러나 인터넷 트래픽이 증가함에 따라 보안 수준이 낮아졌고 다른 인증 방식으로 대체되는 경우가 많아졌습니다. libcurl은 다양한 인증 방법을 지원하기 때문에 다른 인증 방식을 사용하고 싶다면 다른 옵션을 추가하여 요청을 보낼 수 있습니다. 

## 관련 자료:
- [libcurl 공식 문서](https://curl.haxx.se/libcurl/c/curl_easy_setopt.html)
- [HTTP 인증 방식 비교](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)