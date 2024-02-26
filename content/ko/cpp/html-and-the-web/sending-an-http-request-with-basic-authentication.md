---
date: 2024-01-20 18:01:20.579854-07:00
description: "HTTP \uAE30\uBCF8 \uC778\uC99D\uC73C\uB85C \uC694\uCCAD\uC744 \uBCF4\
  \uB0B4\uB294 \uAC83\uC740, \uD074\uB77C\uC774\uC5B8\uD2B8\uAC00 \uC11C\uBC84\uC5D0\
  \ \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\uB97C \uC804\uC1A1\
  \uD558\uC5EC \uC790\uC2E0\uC744 \uC778\uC99D\uD558\uB294 \uBC29\uC2DD\uC785\uB2C8\
  \uB2E4. \uC774\uB294 \uBCF4\uC548\uC774 \uD544\uC694\uD55C \uB370\uC774\uD130\uC5D0\
  \ \uC811\uADFC\uD560 \uB54C \uC0AC\uC6A9\uB429\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-02-25T18:49:52.653374-07:00'
model: gpt-4-1106-preview
summary: "HTTP \uAE30\uBCF8 \uC778\uC99D\uC73C\uB85C \uC694\uCCAD\uC744 \uBCF4\uB0B4\
  \uB294 \uAC83\uC740, \uD074\uB77C\uC774\uC5B8\uD2B8\uAC00 \uC11C\uBC84\uC5D0 \uC0AC\
  \uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\uB97C \uC804\uC1A1\uD558\
  \uC5EC \uC790\uC2E0\uC744 \uC778\uC99D\uD558\uB294 \uBC29\uC2DD\uC785\uB2C8\uB2E4\
  . \uC774\uB294 \uBCF4\uC548\uC774 \uD544\uC694\uD55C \uB370\uC774\uD130\uC5D0 \uC811\
  \uADFC\uD560 \uB54C \uC0AC\uC6A9\uB429\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
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
