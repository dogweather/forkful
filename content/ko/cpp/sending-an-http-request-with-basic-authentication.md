---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:01:20.579854-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 기본 인증으로 요청을 보내는 것은, 클라이언트가 서버에 사용자 이름과 비밀번호를 전송하여 자신을 인증하는 방식입니다. 이는 보안이 필요한 데이터에 접근할 때 사용됩니다.

## How to: (어떻게:)
```C++
#include <iostream>
#include <curl/curl.h>
#include <string>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        const std::string username = "user";
        const std::string password = "pass";
        std::string credentials = username + ":" + password;

        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, credentials.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);

        res = curl_easy_perform(curl);

        if(res != CURLE_OK) {
            std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
        } else {
            std::cout << "Output: " << readBuffer << std::endl;
        }

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```
Sample Output:
```
Output: {"status":"success","data":"Authenticated Data Here."}
```

## Deep Dive (심층 분석)
HTTP 기본 인증은 HTTP/1.0 부터 있었으며, 가장 단순하고 구현하기 쉬운 인증 방식입니다. 하지만, 보안이 중요한 애플리케이션에서는 기본 인증을 지양하고 SSL/TLS 같은 프로토콜과 결합하여 사용하면 좋습니다. OAuth 같은 토큰 기반 인증은 더 안전한 대안으로 자리잡고 있습니다. C++에서는 libcurl이라는 라이브러리를 사용하여 HTTP 요청을 보내고, 인증을 처리할 수 있습니다. 이 코드 예제는 libcurl을 사용하여 기본 인증이 포함된 HTTP GET 요청을 보낸 방법을 보여줍니다.

## See Also (더보기)
- cURL 공식 문서: [https://curl.se/libcurl/](https://curl.se/libcurl/)
- cURL에 대한 기본 인증 가이드: [https://curl.se/docs/httpscripting.html#Basic_authentication](https://curl.se/docs/httpscripting.html#Basic_authentication)
- HTTP 인증에 대한 더 깊은 정보: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
