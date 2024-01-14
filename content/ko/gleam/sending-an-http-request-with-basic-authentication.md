---
title:                "Gleam: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

기본 인증을 사용하여 HTTP 요청을 보내는 이유는 무엇일까요? 간단하게 알아보겠습니다.

## 방법

기본 인증을 사용하여 HTTP 요청을 보내는 방법을 알아보겠습니다. 먼저 Gleam으로 HTTP 요청을 보내는 방법은 아래의 코드 블록과 같습니다.

```Gleam
let authorization = "Basic TW9uZOppa3NlOWxmdXJrOnBhc3N3b3Jk"
let endpoint = "https://example.com/api"
let req_body = "name=Gleam&age=5"
let headers = [("Content-Type", "application/x-www-form-urlencoded"),
                ("Authorization", authorization)
                ]
let result = Http.send{method: "POST", url: endpoint, headers: headers, body: req_body}
```

위의 코드 블록은 예시로, Gleam에서 기본 인증을 사용하여 HTTP POST 요청을 보내는 방법을 보여줍니다. 헤더에는 인증 정보를 담고 있는 `"Authorization"` 필드가 포함되어 있어야 합니다. 이 방법으로 우리는 외부 API와 통신할 수 있습니다.

위의 코드를 실행하면, `result` 변수에는 요청에 대한 응답 정보가 담겨있습니다. 예를 들어, 성공하는 경우 `result`는 아래와 같은 값이 담겨있을 것입니다.

```Gleam
{status: 200,
 headers: [...],
 body: "{ "success": true, "message": "Request successful" }"
}
```

이렇게 응답 정보를 받아온 후, 우리는 필요한 작업을 할 수 있게 됩니다.

## 깊이 알아보기

기본 인증을 사용하여 HTTP 요청을 보내는 것은 서버와 클라이언트 간의 권한 인증 방식 중 하나입니다. 이를 통해 우리는 클라이언트의 신원을 확인할 수 있고, 요청이 유효한지를 판단할 수 있습니다.

기본 인증은 아래의 방식을 따릅니다.

1. 클라이언트는 서버로 요청을 보냅니다.
2. 서버는 클라이언트의 인증 정보를 확인합니다.
3. 인증 정보가 올바르지 않으면, 서버는 요청을 거부합니다.
4. 인증 정보가 올바를 경우, 서버는 요청을 수행하고 응답을 보냅니다.

따라서 기본 인증을 사용하여 보안을 강화할 수 있습니다.

## 관련 정보

* [Gleam 공식 문서](https://gleam.run/)
* [HTTP 모듈 관련 정보](https://gleam.run/build-in/http.html)
* [HTTP 요청에 대한 더 깊은 정보](https://www.tutorialspoint.com/http/http_authentication.htm)