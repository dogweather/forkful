---
title:                "Gleam: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청을 보내는 것에 참여하는 이유는 무엇인가요? 간단히 알아보겠습니다.

HTTP 요청은 인터넷에서 데이터를 주고받기 위한 표준 프로토콜입니다. 예를 들어, 웹 브라우저는 웹 사이트의 내용을 요청하고, 서버는 해당 내용을 전송합니다. Gleam에서 HTTP 요청을 보내는 것은 데이터를 가져오거나 전송하기 위한 중요한 방법입니다. 그래서 우리가 이렇게 중요하고 유용한 기능을 배우는 것입니다.

## 방법
"```Gleam
import gleam/http

let response = http.request(url: "https://koreanblog.com")
|> http.send
```"
위의 예제에서 우리는 Gleam의 `http` 모듈을 사용하여 HTTP 요청을 보내고 서버로부터의 응답을 받아왔습니다. `url` 파라미터에서는 요청을 보낼 서버의 주소를 지정할 수 있습니다. 또한 HTTP 메서드, 요청 본문, 헤더 등을 추가로 지정할 수 있습니다. 위의 예제에서는 간단하게 요청을 보내고 응답을 받아오기만을 보여주었지만, 자세한 사용 방법은 Gleam 공식 문서를 참고하세요.

위의 코드를 실행하면 `response` 변수에 응답 객체가 할당됩니다. 이 객체에서는 HTTP 응답의 상태 코드, 응답 본문 등 다양한 정보를 얻을 수 있습니다. 예를 들어, `response.body`를 통해 본문을 얻을 수 있습니다. 그리고 `response.body.string()`과 같이 메서드를 이용하면 본문을 문자열로 변환할 수 있습니다.

## 심층 탐구
HTTP 요청을 보내는 것은 매우 중요하고 유용하지만, 실제로는 그보다 더 복잡한 작업을 수행할 수 있습니다. 예를 들어, HTTP 요청을 보낼 때 인증 정보를 함께 보낼 수도 있습니다. 이를 위해서는 `http.send` 함수에 새로운 파라미터를 추가하여 인증 정보를 지정하면 됩니다. 또한 HTTP 요청에 대한 응답을 처리할 때에서도, 에러 처리와 같은 상황에서는 Gleam의 예외 처리 기능을 사용할 수 있습니다. 이 외에도 다양한 기능과 라이브러리를 이용해 보다 효율적으로 HTTP 요청을 보낼 수 있습니다.

## 참고 자료
- [Gleam 공식 문서](https://gleam.run/documentation/)
- [HTTP 프로토콜 설명 (Wikipedia)](https://ko.wikipedia.org/wiki/HTTP)
- [Gleam에서 HTTP 요청 보내기 (블로그 포스트)](https://medium.com/@gleam/new-to-gleam-sending-http-requests-7dbeaee99c66)