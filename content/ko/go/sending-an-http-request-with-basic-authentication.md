---
title:                "Go: HTTP 요청에 기본 인증 정보를 보내는 방법"
simple_title:         "HTTP 요청에 기본 인증 정보를 보내는 방법"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증과 함께 보내는 것이 왜 중요할까요? 일반적으로 이는 웹 애플리케이션에서 사용자 인증에 필요합니다. 인증은 서버와 클라이언트 간의 암호화된 통신을 가능하게 하며, 이를 통해 중요한 정보를 안전하게 전송할 수 있습니다.

## 하는 방법

Go 언어로 HTTP 요청에 기본 인증을 추가하는 방법을 알아보겠습니다. 먼저 `http` 패키지를 가져옵니다. 그리고 `http.NewRequest` 함수를 사용하여 새로운 요청을 생성합니다.

```Go
req, err := http.NewRequest("GET", "https://example.com/api/users", nil)
```

이 함수는 요청 메서드(GET, POST 등), URL, 그리고 요청 바디를 매개변수로 받습니다. 이 예시에서는 바디를 전송하지 않기 때문에 `nil`을 전달하였습니다.

다음으로, `SetBasicAuth` 메서드를 사용하여 기본 인증을 추가합니다. 이 메서드는 요청에 인증 정보를 인코딩하고 `Authorization` 헤더를 추가합니다.

```Go
req.SetBasicAuth("username", "password")
```

마지막으로, `http.Client`에서 생성한 클라이언트를 사용하여 요청을 보냅니다.

```Go
client := http.Client{}
resp, err := client.Do(req)
```

`resp` 변수에는 `http.Response` 객체가 반환됩니다. 이를 통해 요청의 상태 코드, 헤더, 바디 등에 대한 정보를 확인할 수 있습니다.

## 깊이 파고들기

HTTP 요청에 기본 인증을 추가하는 것은 클라이언트에서 요청을 보낼 때 사용자의 인증 정보를 서버로 전송하기 위해 사용됩니다. 이는 보안적인 측면에서 중요한 역할을 합니다. 서버에서는 이 정보를 확인하여 사용자가 유효한지 확인하고, 그에 따른 액션을 취할 수 있습니다.

또한, 인증 정보는 암호화되어 전송되기 때문에 제3자가 해당 정보를 엿볼 수 없습니다. 이를 통해 중요한 정보를 안전하게 전송할 수 있습니다.

## 더 알아보기

- [The Go Blog: HTTP Clients in Go](https://blog.golang.org/http-clients)
- [GoDoc: http package](https://golang.org/pkg/net/http/)
- [MDN Web Docs: Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [RFC 7617: The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)