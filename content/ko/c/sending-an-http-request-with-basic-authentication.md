---
title:                "C: 기본 인증을 사용한 http 요청 보내기"
simple_title:         "기본 인증을 사용한 http 요청 보내기"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청을 보낼 때 기본 인증을 사용하는 이유는 보안을 위해서입니다. 기본 인증은 사용자 이름과 비밀번호를 인증하기 위해 매우 간단하게 사용할 수 있지만, 암호화되지 않은 형태로 전송되므로 완벽한 보안은 아닙니다.

## 어떻게
C 언어를 이용하여 기본 인증을 사용해 HTTP 요청을 보내는 방법을 알아보겠습니다. 먼저 필요한 헤더 파일을 포함시킵니다.

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>
```

다음으로 기본 인증에 필요한 사용자 이름과 비밀번호를 변수에 저장합니다.

```
char *username = "사용자 이름";
char *password = "비밀번호";
```

그리고 HTTP 요청을 보낼 URL을 변수에 저장합니다.

```
char *url = "요청을 보낼 URL";
```
curl 라이브러리를 이용하여 기본 인증을 추가한 HTTP 요청을 보내는 함수를 만듭니다.

```
void sendRequest(char *url, char *username, char *password) {
  // curl 객체 생성
  CURL *curl;
  // HTTP 요청을 위한 변수 선언
  CURLcode res;
 
  // curl 객체 초기화
  curl = curl_easy_init();
  if(curl) {
    // URL 설정
    curl_easy_setopt(curl, CURLOPT_URL, url);
    // 기본 인증 설정
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
    // 사용자 이름과 비밀번호 설정
    curl_easy_setopt(curl, CURLOPT_USERNAME, username);
    curl_easy_setopt(curl, CURLOPT_PASSWORD, password);

    // HTTP 요청 보내기
    res = curl_easy_perform(curl);
    
    // 에러 체크
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

    // curl 객체 정리
    curl_easy_cleanup(curl);
  }
}
```

마지막으로 함수를 호출하여 HTTP 요청을 보내고 결과를 출력합니다.

```
int main() {
  sendRequest(url, username, password);

  return 0;
}
```

```
HTTP/1.1 200 OK
Server: Apache/2.2.14 (Unix)
Cache-Control: no-store, no-cache, must-revalidate, post-check=0, pre-check=0
Expires: Thu, 19 Nov 1981 08:52:00 GMT
Pragma: no-cache
Content-Length: 4056
Content-Type: text/html

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
...

``` 

## 깊게 들어가보기
기본 인증은 HTTP 요청에 대한 인증 정보를 포함하는 가장 간단한 방법 중 하나입니다. 하지만 이는 암호화되지 않은 형태로 인증 정보가 전송되므로 완벽한 보안은 아닙니다. 따라서 더 강력한 인증 방식을 위해서는 HTTPS와 같은 암호화된 프로토콜을 사용해야 합니다.

또한 기본 인증의 경우 사용자 이름과 비밀번호가 평문으로 전송되므로 중간에 가로채는 것이 매우 쉽습니다. 따라서 중요한 정보를 전송할 때에는 다른 방법을 사용하는 것이 좋습니다.

## 관련 링크
- [curl 라이브러리 공식 문서](https://curl.haxx.se/libcurl/)
- [HTTP 메서드에 대한 자세한 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Methods)
- [HTTPS를 이용한 보안 통신 방법](https://en.wikipedia.org/wiki/HTTPS