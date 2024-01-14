---
title:                "Go: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 이유는 다양하지만 대개 서버로부터 데이터를 가져오고 싶기 때문입니다. 예를 들어 웹 애플리케이션에서는 사용자의 입력을 기반으로 서버에서 데이터를 가져와서 동적으로 웹 페이지를 생성할 수 있습니다. 또는 API를 사용하여 다른 서버의 정보를 가져올 수도 있습니다. 어떤 이유든지 이해하는 것은 중요합니다.

## 하는 방법

HTTP 요청을 보내는 것은 Go 언어에서 간단한 작업입니다. 먼저 `net/http` 패키지를 임포트하고, 요청을 보낼 URL을 작성합니다.

```Go
import "net/http"

// 요청을 보낼 URL
url := "https://example.com"

// HTTP GET 방식으로 요청 보내기
resp, err := http.Get(url)

// 에러 처리
if err != nil {
    // 요청 보내기 실패
}

// 요청에 대한 응답 출력하기
fmt.Println(resp)
```

위 코드에서는 `net/http` 패키지의 `Get` 함수를 사용하여 `url` 변수에 저장된 URL로 GET 방식으로 요청을 보내고, 그 결과를 `resp` 변수에 저장합니다. 요청이 성공하면 `resp` 변수에는 요청에 대한 응답에 대한 정보가 담겨 있을 것입니다.

또 다른 예제로 HTTP POST 방식으로 요청을 보내는 방법을 보겠습니다.

```Go
import "net/http"

// 요청을 보낼 URL
url := "https://example.com"

// HTTP POST 방식으로 보낼 데이터
data := []byte(`{"message": "Hello World!"}`)

// 데이터를 포함하여 요청 보내기
resp, err := http.Post(url, "application/json", bytes.NewBuffer(data))

// 에러 처리
if err != nil {
    // 요청 보내기 실패
}

// 요청에 대한 응답 출력하기
fmt.Println(resp)
```

위 코드에서는 `net/http` 패키지의 `Post` 함수를 사용하여 `url` 변수에 저장된 URL로 POST 방식으로 요청을 보내고, `data` 변수에 저장된 데이터를 요청에 포함시킵니다. 요청이 성공하면 `resp` 변수에는 요청에 대한 응답에 대한 정보가 담겨 있을 것입니다.

## 더 깊숙히 들어가기

HTTP 요청의 여러 가지 개념과 기능을 이해하는 것은 이 시대의 개발자로서 중요한 역량입니다. 예를 들어 HTTP 요청에 대한 응답 코드를 이해하는 것은 문제 해결에 큰 도움이 될 수 있습니다. 또는 요청에 헤더를 포함하는 방법을 알고 있다면 보안적으로 더 안전한 요청을 보낼 수 있을 것입니다.

## 관련 정보

- [https://golang.org/pkg/net/http/](https://golang.org/pkg/net/http/)
- [https://www.w3schools.com/tags/ref_httpmessages.asp](https://www.w3schools.com/tags/ref_httpmessages.asp)
- [https://developer.mozilla.org/ko/docs/Web/HTTP/Status](https://developer.mozilla.org/ko/docs/Web/HTTP/Status)