---
date: 2024-01-20 18:01:20.579854-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) HTTP \uAE30\uBCF8 \uC778\uC99D\uC740 HTTP/1.0\
  \ \uBD80\uD130 \uC788\uC5C8\uC73C\uBA70, \uAC00\uC7A5 \uB2E8\uC21C\uD558\uACE0 \uAD6C\
  \uD604\uD558\uAE30 \uC26C\uC6B4 \uC778\uC99D \uBC29\uC2DD\uC785\uB2C8\uB2E4. \uD558\
  \uC9C0\uB9CC, \uBCF4\uC548\uC774 \uC911\uC694\uD55C \uC560\uD50C\uB9AC\uCF00\uC774\
  \uC158\uC5D0\uC11C\uB294 \uAE30\uBCF8 \uC778\uC99D\uC744 \uC9C0\uC591\uD558\uACE0\
  \ SSL/TLS \uAC19\uC740 \uD504\uB85C\uD1A0\uCF5C\uACFC \uACB0\uD569\uD558\uC5EC \uC0AC\
  \uC6A9\uD558\uBA74 \uC88B\uC2B5\uB2C8\uB2E4. OAuth \uAC19\uC740 \uD1A0\uD070 \uAE30\
  \uBC18\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.919359-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) HTTP \uAE30\uBCF8 \uC778\uC99D\uC740 HTTP/1.0 \uBD80\
  \uD130 \uC788\uC5C8\uC73C\uBA70, \uAC00\uC7A5 \uB2E8\uC21C\uD558\uACE0 \uAD6C\uD604\
  \uD558\uAE30 \uC26C\uC6B4 \uC778\uC99D \uBC29\uC2DD\uC785\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

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
