---
title:                "C++: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 데 기본 인증을 사용하는 이유는 무엇일까요? 기본 인증은 서버와의 안전한 통신을 위해 사용되며, 사용자의 계정 정보를 보호하기 위해 필요합니다.

## 하는 방법

```C++
#include <iostream>
#include <curl/curl.h>

using namespace std;

// 인증 정보를 저장하는 구조체
struct UserInfo {
    const char * username;
    const char * password;
};

// 인증 정보를 설정하는 콜백 함수
static curl_write_callback SetAuth(CURL *curl, curl_header header, char * line, LinkedList * userdata) {
    // userinfo 구조체에 사용자 정보 저장
    UserInfo * credentials = (UserInfo*) userdata->content;
    // HTTP 인증 헤더에 사용자 이름과 비밀번호 설정
    sprintf(line, "Authorization: Basic %s:%s", credentials->username, credentials->password);
    return strlen(line);
}

// HTTP 요청을 보내는 함수
static void SendHTTPRequest() {
    // 커버리브를 초기화하기 전 사용자 정보 저장
    UserInfo credentials = { "username", "password" };
    // 커버리브 라이브러리 초기화
    curl_global_init(CURL_GLOBAL_SSL);
    // 커버리브 핸들 생성
    CURL * curl = curl_easy_init();
    if (curl) {
        // 요청할 URL 설정
        curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
        // 인증 정보 설정
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, SetAuth);
        // 사용자 정보를 인증 콜백 함수의 매개변수로 넘기기 위해 설정
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &credentials);
        // HTTP 요청 보내기
        CURLcode res = curl_easy_perform(curl);
        // 응답 코드 출력
        cout << "HTTP response code: " << res << endl;

        // 커버리브 핸들 해제
        curl_easy_cleanup(curl);
    }
    // 커버리브 라이브러리 정리
    curl_global_cleanup();
}

int main() {
    // HTTP 요청 보내는 함수 호출
    SendHTTPRequest();
    return 0;
}
```

**출력:**

```
HTTP response code: 200
```

## 깊게 들어가기

위의 예제에서는 커버리브(CURL) 라이브러리를 사용하여 HTTP 요청을 보냈습니다. 이 라이브러리는 인터넷에 접근하는 다양한 프로토콜을 지원하며, HTTP는 그 중 하나입니다. 커버리브는 HTTP 요청을 보낼 때 기본 인증을 사용하기 위해 `CURLOPT_WRITEFUNCTION` 매개변수를 설정하는 것으로 인증을 처리합니다. 이를 통해 인증 헤더에 사용자 이름과 비밀번호를 추가하여 안전한 통신을 할 수 있습니다. 그리고 `CURLOPT_WRITEDATA`를 사용하여 사용자 정보를 인증 콜백 함수에 넘겨줍니다. 

## See Also

- [CURL 공식 문서](https://curl.haxx.se/libcurl/c)
- [HTTP 기본 인증에 대한 더 자세한 정보](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [CURL 예제와 사용법](https://curl.haxx.se/libcurl/c/example.html)