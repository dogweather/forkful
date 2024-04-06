---
date: 2024-01-20 17:43:30.464721-07:00
description: "How to: (\uBC29\uBC95:) \uCD9C\uB825 \uC608\uC81C."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.299248-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
weight: 42
---

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
