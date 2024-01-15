---
title:                "HTTP 요청 보내기"
html_title:           "C++: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것에 참여하는 이유는 다양합니다. 예를 들어, API를 사용하여 웹 서비스와 상호 작용하거나 데이터를 검색하고 처리하기 위해 서버와 통신할 때 HTTP 요청을 보내는 것이 일반적입니다.

## 방법

아래의 코드 블록들은 C++로 HTTP 요청을 보내는 방법을 보여줍니다. 이 예제는 전통적인 GET 요청과 POST 요청을 포함하고 있습니다. 또한, 각 코드 블록 다음에는 해당 요청을 보낸 후 받은 응답의 예시가 제공됩니다.

```C++
// GET 요청 코드 예시
#include <iostream>
#include <curl/curl.h> // curl 라이브러리 포함

int main(){
    CURL *curl;
    CURLcode res;
    curl = curl_easy_init(); // curl 초기화
    if(curl){
        curl_easy_setopt(curl, CURLOPT_URL, "https://example.com"); // 요청할 URL 설정
        res = curl_easy_perform(curl); // 요청 보내기
        if(res != CURLE_OK){ // 요청이 정상적으로 이루어지지 않으면 에러 출력
            std::cout << "Error: " << curl_easy_strerror(res) << std::endl;
        }
        curl_easy_cleanup(curl); // curl 리소스 정리
    }
    return 0;
}

// GET 요청 응답 예시
<html>
    <head>
        <title>Example Domain</title>
    </head>
    <body>
        <h1>Example Domain</h1>
        <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
        <p><a href="https://www.iana.org/domains/example">More information...</a></p>
    </body>
</html>

```

```C++
// POST 요청 코드 예시
#include <iostream>
#include <curl/curl.h> // curl 라이브러리 포함

int main(){
    CURL *curl;
    CURLcode res;
    curl = curl_easy_init(); // curl 초기화
    if(curl){
        curl_easy_setopt(curl, CURLOPT_URL, "https://example.com"); // 요청할 URL 설정
        curl_easy_setopt(curl, CURLOPT_POST, 1L); // POST 요청 설정
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "param1=value1&param2=value2"); // POST 파라미터 설정
        res = curl_easy_perform(curl); // 요청 보내기
        if(res != CURLE_OK){ // 요청이 정상적으로 이루어지지 않으면 에러 출력
            std::cout << "Error: " << curl_easy_strerror(res) << std::endl;
        }
        curl_easy_cleanup(curl); // curl 리소스 정리
    }
    return 0;
}

// POST 요청 응답 예시
<html>
    <head>
        <title>Example Domain</title>
    </head>
    <body>
        <h1>Example Domain</h1>
        <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
        <p><a href="https://www.iana.org/domains/example">More information...</a></p>
    </body>
</html>
```

## 깊게 들어가기

HTTP 요청은 인터넷에서 데이터를 주고받을 때 이용되는 프로토콜 중 하나입니다. 이는 웹 서버와 클라이언트가 상호 작용하는 과정에서 가장 중요한 단계 중 하나이며, HTTP 메소드(GET, POST 등)를 통해 특정 리소스에 접근하고, 해당 리소스를 읽거나 수정하거나 삭제할 수 있도록 합니다. HTTP 요청의 헤더에는 요청의 목적과 함께 필요한 정보들이 포함되어 있으며, 요청의 방식에 따라 GET과 POST 메소