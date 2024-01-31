---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:01:06.436633-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"

category:             "C"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청을 보내면 서버와 데이터를 주고받는다. 기본 인증이란, 사용자 이름과 비밀번호를 Base64로 인코딩해서 요청에 추가하는 거다. 이렇게 하면 안전하게 서버에 로그인해서 권한이 필요한 데이터에 접근할 수 있다.

## How to (방법)
```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl) {
        // 서버 URL 설정
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // 기본 인증 설정: "username:password"
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
        
        // 요청 실행 및 결과 처리
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        // 마무리
        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();
    return 0;
}
```
Sample Output:
```
<html>
<body>
    <p>Authorized resource access granted.</p>
</body>
</html>
```

## Deep Dive (심층 분석)
HTTP 기본 인증은 단순하지만 오래된 방법이다. RFC 7617 문서에서 정의되었다. 현대에는 더 안전한 대체 방법들이 있어, 토큰 기반 인증 같은 OAuth를 많이 사용한다. 그럼에도 불구하고, API가 간단하고 트래픽이 암호화된 경우에는 기본 인증이 여전히 유용하다. 구현할 때는 `libcurl` 전송 라이브러리처럼 안전하고 유지보수가 쉬운 라이브러리를 사용하는 것이 좋다.

## See Also (관련 자료)
- cURL 바로가기: [cURL](https://curl.se/)
- RFC 7617, HTTP Basic Authentication: [RFC 7617](https://datatracker.ietf.org/doc/html/rfc7617)
- OAuth 2.0: [OAuth 2.0](https://oauth.net/2/)
