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

## 왜

HTTP 요청을 기본 인증으로 보내는 과정에 참여하는 이유는 여러 가지가 있습니다. 예를 들어, 사용자가 웹사이트에 로그인한 다음 해당 사용자의 데이터에 접근하려면, 서버는 사용자의 인증 정보를 확인할 필요가 있습니다. 이때 기본 인증은 가장 간단하고 효과적인 방법입니다.

## 어떻게

보안 강화를 위해 기본 인증을 사용하는 방법은 다음과 같습니다.
```
#include <curl/curl.h>

// 기본 인증 헤더 구성
struct curl_slist *headers = NULL;
headers = curl_slist_append(headers, "Authorization: Basic <username:password>");

// cURL 초기화
CURL *curl_handle;
curl_handle = curl_easy_init();

if (curl_handle) {
    // 요청 URL 설정
    curl_easy_setopt(curl_handle, CURLOPT_URL, "https://example.com");

    // 헤더 추가
    curl_easy_setopt(curl_handle, CURLOPT_HTTPHEADER, headers);

    // 기본 인증 설정
    curl_easy_setopt(curl_handle, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);

    // 요청 보내기
    curl_easy_perform(curl_handle);

    // cURL 해제
    curl_easy_cleanup(curl_handle);
}

// 헤더 메모리 해제
curl_slist_free_all(headers);
```

위 코드를 실행하면 서버로부터 다음과 같은 응답을 받게 될 것입니다.
```
HTTP/1.1 200 OK
Date: Tue, 14 Sep 2021 00:00:00 GMT
Server: Apache
Content-Length: 234

<html>
    <head>
        <title>Example Website</title>
    </head>
    <body>
        <h1>Welcome!</h1>
        <p>You have successfully logged in.</p>
    </body>
</html>
```

## 더 깊게

HTTP 기본 인증은 base64로 인코딩된 사용자 이름과 비밀번호를 인증 요청 헤더에 포함하여 요청을 보냅니다. 이때 base64는 암호화가 아니므로 보안 수준이 낮습니다. 따라서 보안이 더 필요한 경우 토큰 기반 인증을 고려해야 합니다.

## 참고문서

- [cURL 공식 문서](https://curl.se/libcurl/c/curl_easy_setopt.html)
- [HTTP 기본 인증에 대한 RFC](https://datatracker.ietf.org/doc/html/rfc7617)