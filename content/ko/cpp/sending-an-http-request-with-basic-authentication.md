---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "C++: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# "무엇이며 왜 하는가?"

HTTP 요청을 보낼 때 일반 인증을 사용하는 것은 웹에서 보안을 유지하는 데 중요합니다. 일반 인증은 사용자가 아이디와 비밀번호를 입력하면 서버에 인증 정보를 제공해주는 방식으로, 이를 통해 안전하게 데이터를 주고받을 수 있습니다. 프로그래머들은 이를 사용하여 데이터의 보안과 무결성을 유지할 수 있습니다.

# "하는 방법:"

```C++
#include <iostream>
#include <curl/curl.h>

using namespace std;

// 인증 정보 구조체
struct auth {
  const char *username;
  const char *password;
};

// HTTP 요청을 보내는 함수
int send_http_request(const char *url, struct auth credentials) {

  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if (curl) {
    // URL 설정
    curl_easy_setopt(curl, CURLOPT_URL, url);
    // 일반 인증 추가
    curl_easy_setopt(curl, CURLOPT_USERNAME, credentials.username);
    curl_easy_setopt(curl, CURLOPT_PASSWORD, credentials.password);
    // 요청 보내기
    res = curl_easy_perform(curl);
    // 성공적으로 요청을 보냈는지 확인
    if (res != CURLE_OK)
      cerr << "HTTP 요청 실패: " << curl_easy_strerror(res) << endl;
    // curl 핸들 닫기
    curl_easy_cleanup(curl);
    return 1;
  }
  else {
    cerr << "curl 초기화 실패." << endl;
    return 0;
  }
}

int main() {

  // 보낼 URL
  const char *url = "https://www.example.com";
  // 인증 정보 설정
  struct auth credentials = {"username", "password"};

  // HTTP 요청 보내기
  int result = send_http_request(url, credentials);
  return 0;
}
```

## 더 깊이 파보기:

HTTP 요청에 대해 자세히 알아보면, 클라이언트와 서버 간의 통신 방법으로 월드 와이드 웹에서 널리 사용됩니다. 일반 인증은 첫 번째 HTTP 인증 방식으로 1999년에 RFC 2617로 정의되었습니다. 그 후 개발자들은 추가적인 인증 방식을 만들었지만, 일반 인증은 여전히 널리 사용되고 있습니다.

대체로 다른 인증 방식으로는 다이제스트 인증이 있고, 보안을 더욱 강화한 베어러 인증도 있습니다. 하지만 많은 웹 서비스에서 일반 인증이 표준으로 사용되고 있으며, 애플리케이션에서 일반 인증을 사용하면 성능 저하가 적으므로 더 많이 쓰입니다.

C++에서 일반 인증을 구현하기 위해 다양한 라이브러리를 사용할 수 있지만, 가장 대표적인 것은 libcurl입니다. libcurl은 다양한 프로토콜을 지원하여 HTTP 요청 뿐만 아니라 다른 요청들도 쉽게 처리할 수 있도록 만들어져 있습니다.

## 관련 소스보기:

- [libcurl 공식 페이지](https://curl.haxx.se/libcurl/)
- [libcurl을 사용한 HTTP 요청 보내기 예제](https://curl.haxx.se/libcurl/c/example.html)
- [HTTP 인증 방식의 RFC 문서](https://tools.ietf.org/html/rfc2617)