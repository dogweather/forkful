---
title:                "기본 인증으로 http 요청 보내기"
html_title:           "C++: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

***HTTP 요청**을 본문에 담고 있는 `Basic Authentication`으로 전송하는 이유는 무엇일까요? 아주 간단합니다. `Basic Authentication`은 *사용자 인증*을 위한 가장 기본적인 방법 중 하나입니다. 즉, 서버와 클라이언트 간의 안전한 커뮤니케이션을 보장하기 위함입니다.

## 하는 법

아래의 예시 코드를 참고하여 `Basic Authentication`으로 `HTTP 요청`을 전송하는 방법을 알아보겠습니다.

```C++
#include <iostream>
#include <curl/curl.h>

// 예시 URL
#define URL "https://www.example.com"

// 사용자 이름과 비밀번호
#define USERNAME "my_username"
#define PASSWORD "my_password"

int main() {
  // libcurl 초기화
  curl_global_init(CURL_GLOBAL_ALL);

  // 새로운 curl 세션 생성
  CURL *curl = curl_easy_init();

  // URL 설정
  curl_easy_setopt(curl, CURLOPT_URL, URL);

  // Basic Authentication 설정
  curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
  curl_easy_setopt(curl, CURLOPT_USERNAME, USERNAME);
  curl_easy_setopt(curl, CURLOPT_PASSWORD, PASSWORD);

  // 요청 전송
  CURLcode res = curl_easy_perform(curl);
  if (res != CURLE_OK) {
    std::cerr << "Request failed." << std::endl;
  }

  // curl 세션 종료
  curl_easy_cleanup(curl);

  // libcurl 해제
  curl_global_cleanup();

  return 0;
}
```

위의 코드를 실행하면 `URL`에 해당하는 웹사이트에 `Basic Authentication`을 포함한 `HTTP 요청`이 전송됩니다.

## 깊게 들어가기

`Basic Authentication`은 `HTTP 요청`의 **Authorization** 헤더를 사용하여 인증 정보를 전송합니다. 즉, 클라이언트가 `Authorization` 헤더를 포함하여 서버에 요청을 보냄으로써 사용자 인증을 요청하고, 서버는 해당 사용자의 인증 정보를 확인한 후 요청을 처리하게 됩니다.

이는 보안 측면에서 안전하지 못한 방법이기 때문에 최근에는 `Basic Authentication` 대신 **OAuth**나 **Token-based Authentication** 등 보다 안전한 방법들이 많이 사용되고 있습니다. 하지만 여전히 몇몇 웹사이트들은 `Basic Authentication`을 사용하고 있기 때문에 알고 있으면 유용할 수 있습니다.

## 참조

* [curl - Basic Authentication](https://curl.haxx.se/libcurl/c/CURLOPT_HTTPAUTH.html)
* [HTTP Authentication](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication)