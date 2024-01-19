---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

HTTP 요청을 보내는 것은 웹 서버에 정보를 요청하는 프로그래밍 방법입니다. 이렇게 함으로써, 개발자는 API에서 데이터를 받아오거나, 원격 서버에 파일을 업로드하는 등의 동작을 구현할 수 있습니다.

## 어떻게 하는가:

```C++
#include <cpp_httplib/httplib.h>

int main() {
    httplib::Client cli("http://example.com");
    auto res = cli.Get("/");
    if (res && res->status == 200)
        std::cout << res->body << std::endl;
    return 0;
}
```

이 코드는 `example.com` 웹 서버에 HTTP GET 요청을 보내는 예제입니다. 요청이 성공하면, 웹 서버의 응답 내용을 출력합니다.

## Deep Dive

HTTP 요청은 월드 와이드 웹의 기원과 밀접하게 관련되어 있습니다. 초기 인터넷 시절, 사용자는 웹 서버에 정보를 요청하는 데 HTTP 요청을 사용했습니다. 2022년 현재, 개발자들은 여전히 이 방식을 사용하여 웹 서버와 통신합니다.

대안으로는 다른 종류의 네트워크 프로토콜, 예를 들어 FTP나 SMTP를 사용하는 것이 있습니다. 그러나 이들은 각각 특정한 케이스인 파일 전송과 이메일 전송에 주로 사용됩니다.

C++에서 HTTP 요청을 보내는 방법에는 여러 가지가 있습니다. 위의 예는 `cpp-httplib`라는 라이브러리를 사용하지만, `Boost.Asio`나 `CURLpp` 같은 라이브러리를 사용할 수도 있습니다. 선택할 라이브러리는 사용자의 필요에 따라 달라집니다.

## 참고 링크

1. [cpp-httplib documentation](https://github.com/yhirose/cpp-httplib)
2. [Boost.Asio documentation](https://www.boost.org/doc/libs/1_77_0/doc/html/boost_asio.html)
3. [CURLpp documentation](http://www.curlpp.org/) 

이 세 라이브러리의 문서에서 더 깊은 이해를 얻을 수 있습니다. 적절한 선택은 프로젝트의 필요와 개인의 선호에 따라 달라집니다.