---
date: 2024-01-20 17:43:30.464721-07:00
description: "\uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD558\uB294\
  \ \uAC83\uC740 \uC778\uD130\uB137\uC5D0\uC11C \uD398\uC774\uC9C0\uC758 \uCF58\uD150\
  \uCE20\uB97C\uAC00\uC838\uC640 \uB0B4 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C \uC0AC\
  \uC6A9\uD560 \uC218 \uC788\uAC8C \uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uC774\uB294\
  \ \uD398\uC774\uC9C0\uC5D0\uC11C \uB370\uC774\uD130\uB97C \uCD94\uCD9C\uD558\uAC70\
  \uB098 \uC6F9 \uC2A4\uD06C\uB798\uD551\uC744 \uD560 \uB54C \uD544\uC694\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.666306-06:00'
model: gpt-4-1106-preview
summary: "\uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD558\uB294 \uAC83\
  \uC740 \uC778\uD130\uB137\uC5D0\uC11C \uD398\uC774\uC9C0\uC758 \uCF58\uD150\uCE20\
  \uB97C\uAC00\uC838\uC640 \uB0B4 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C \uC0AC\uC6A9\
  \uD560 \uC218 \uC788\uAC8C \uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
weight: 42
---

## What & Why? (무엇과 왜?)

웹 페이지를 다운로드하는 것은 인터넷에서 페이지의 콘텐츠를가져와 내 프로그램에서 사용할 수 있게 하는 것입니다. 이는 페이지에서 데이터를 추출하거나 웹 스크래핑을 할 때 필요합니다.

## How to: (방법:)

```C++
#include <iostream>
#include <curl/curl.h>

// 콜백 함수
size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            std::cerr << "Error: " << curl_easy_strerror(res) << std::endl;
        } else {
            std::cout << readBuffer << std::endl;
        }
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

출력 예제:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
<body>
...
</body>
</html>
```

## Deep Dive (심층 분석)

웹 페이지 다운로드는 인터넷 초기부터 있어왔습니다. 초기에는 FTP나 HTTP 프로토콜로 명령 줄에서 직접 다운로드했습니다. 오늘날엔 라이브러리가 있어 쉽게 다운로드합니다. C++에서 `libcurl`은 가장 인기 있는 라이브러리 중 하나입니다. 대안으로는 Poco 라이브러리도 있습니다. `libcurl`은 멀티 프로토콜 지원, 스레딩, 병렬 다운로드 등을 제공합니다.

## See Also (참고 자료)

- cURL 공식 홈페이지: https://curl.se/
- cURL for C++: https://curl.se/libcurl/c/
- Poco Libraries: https://pocoproject.org/
- C++ HTTP 네트워킹에 대한 더 깊은 이해: 
  - https://en.cppreference.com/w/cpp/links/libs
  - https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html
