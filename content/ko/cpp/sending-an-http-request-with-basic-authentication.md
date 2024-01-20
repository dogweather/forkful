---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요? (What & Why?)

HTTP 요청에서 기본 인증을 사용하면, 웹서버가 사용자신원 확인하는 방법입니다. 프로그래머는 이것을 사용하여 보안이 필요한 페이지에 액세스합니다. 

## 어떻게 사용하나요? (How to:)

다음의 C++ 코드의 예제는 libcurl을 사용해 기본 인증을 사용한 HTTP 요청을 보내는 방법을 보여줍니다.

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

// 데이터 콜백 함수
size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp){
    size_t totalSize = size * nmemb;
    userp->append((char*)contents, totalSize);
    return totalSize;
}

// HTTP 요청 함수
void SendHttpRequest(const char* username, const char* password) {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");

        std::string userpwd = std::string(username) + ":" + std::string(password);
        curl_easy_setopt(curl, CURLOPT_USERPWD, userpwd.c_str());

        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);

        res = curl_easy_perform(curl);

        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        } else {
            std::cout << readBuffer << std::endl;
        }

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();
}
```
이 코드를 실행하면, "http://www.example.com" 웹 페이지의 내용이 표시됩니다.

## 깊게 알아봅시다 (Deep Dive)

기본 인증(Basic Authentication)은 가장 오래된 HTTP 인증 방식 중 하나로, 현재 많이 사용되지는 않지만, 간단한 인증 요구 사항에 여전히 사용됩니다. 하지만 기본 인증은 ID와 비밀번호를 Base64 인코딩하여 전송하는 방식이므로, 보안에 취약합니다. 그래서 현재는 보안 강화를 위한 대안 방식으로 OAuth, JWT(Json Web Token) 등이 사용되곤 합니다.

르브컬은 C/C++에서 HTTP 통신을 위해 널리 사용되는 라이브러리 중 하나입니다. 멀티플랫폼을 지원하며, HTTPS를 비롯한 다양한 프로토콜을 지원하고, 다양한 HTTP 인증 방법을 사용할 수 있습니다.

## 더 알아봅시다 (See Also)

- libcurl 공식 사이트 : https://curl.haxx.se/libcurl/c/
- HTTP Basic Authentication 설명 : https://en.wikipedia.org/wiki/Basic_access_authentication
- OAuth 공식 사이트 : https://oauth.net/
- JWT(Json Web Token) 공식 사이트 : https://jwt.io/