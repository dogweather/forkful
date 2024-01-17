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

## 무엇이고 왜?

먼저, HTTP 요청을 보내는 것이 무엇인지 알아보겠습니다. 프로그래머들은 컴퓨터나 서버에게 어떠한 작업을 요청하기 위해 HTTP 요청을 보냅니다. 이는 주로 웹 페이지를 불러오거나, 응답을 받아오는 등의 행위를 포함합니다. 프로그래밍에서 HTTP 요청은 꽤 중요한 개념이며, 우리는 웹 서버와 상호작용하기 위해 이 작업을 실행합니다.

## 방법:

C++로 HTTP 요청을 보내는 방법을 보여드리겠습니다. 아래의 샘플 코드를 보고 그 결과를 확인해보세요.

```C++
// HTTP 요청을 보내기 위한 라이브러리 포함
#include <iostream>
#include <cpprest/http_client.h>

// main 함수
int main()
{
    // http 요청 객체 초기화
    http_client client(U("http://www.example.com/"));

    // GET 메서드를 사용한 요청 보내기
    client.request(methods::GET).then([](http_response response)
    {
        // 요청이 완료되면 결과 출력
        if(response.status_code() == status_codes::OK)
        {
            // 결과를 문자열로 변환
            return response.extract_string();
        }
        else
        {
            // 오류 메시지 반환
            return pplx::task_from_result(std::string("Error"));
        }
    })
    .then([](pplx::task<std::string> previousTask)
    {
        // 출력 결과 확인
        try
        {
            // 이전 태스크의 결과를 가져와서 출력
            std::cout << previousTask.get() << std::endl;
        }
        catch (const http_exception& e)
        {
            // 오류가 발생하면 출력
            std::cout << "Error exception: " << e.what() << std::endl;
        }
    })
    .wait(); // 태스크 완료를 기다림

    return 0;
}
```

위의 코드는 http_client 라이브러리를 사용하여 HTTP 요청을 보내는 방법을 보여줍니다. 이를 통해 우리는 매우 간단하게 웹 서버와 상호작용할 수 있습니다.

## 깊게 파보기:

HTTP 요청은 웹 개발에서 빠질 수 없는 중요한 개념입니다. 인터넷이 발전하면서 HTTP 요청 역시 많은 발전을 거쳐 왔습니다. 예전에는 GET과 POST 메서드만 사용할 수 있었지만, 현재는 PUT, PATCH, DELETE 등 다양한 메서드를 사용할 수 있습니다. 또한, cURL과 같은 다른 도구를 사용해서도 HTTP 요청을 보낼 수 있습니다.

HTTP 요청은 우리가 일상 생활에서 많이 사용하는 인터넷 검색, 쇼핑, 소셜 미디어 등 다양한 서비스의 기초가 됩니다. 따라서 프로그래밍에 관심이 있는 분이라면 HTTP 요청에 대해 알아두는 것이 좋습니다.

## 관련 자료:

- [cpprest sdk](https://github.com/Microsoft/cpprestsdk): HTTP 요청을 보내기 위한 C++ 라이브러리
- [Introduction to HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview): HTTP에 대한 기본 개념 소개
- [HTTP Request Methods](https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html): HTTP 요청의 다양한 메서드에 대한 설명