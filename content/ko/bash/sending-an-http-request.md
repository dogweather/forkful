---
title:                "HTTP 요청 보내기"
html_title:           "Bash: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜
*왜 누군가가 HTTP 요청을 보내는지 설명하는 최대 2문장.*

HTTP 요청은 인터넷 상에서 정보를 교환하는 가장 일반적인 방법 중 하나입니다. 여러분은 웹사이트, 앱, 또는 API와의 상호작용을 통해 다양한 정보를 얻거나 제공하기 위해 HTTP 요청을 보낼 수 있습니다.

## 사용 방법

* ```Bash
  curl <URL>
  ```
  *주어진 URL로 특정 웹 페이지에 접근하는 간단한 방법입니다.

* ```Bash
  curl -X POST -H "Content-Type: application/json" -d '{"username": "John", "password": "123"}' <URL>
  ```
  *HTTP POST 요청을 보내는 더 복잡한 예제입니다. 헤더와 본문 내용을 함께 포함해야 할 때 유용합니다.

### 출력 예시

* ```Bash
  <HTTP response>
  ```
  *서버로부터 받은 HTTP 응답을 출력하는 방법입니다. 응답에는 상태 코드, 헤더, 본문 등이 포함될 수 있습니다.

## 깊이 파고들기

HTTP 요청은 다양한 메서드를 사용할 수 있으며 각 메서드는 다른 목적을 가지고 있습니다. GET 메서드는 웹페이지를 가져오는 데 사용되고, POST 메서드는 데이터를 서버에 제출하기 위해 사용됩니다. 또한 HTTP 요청은 헤더를 사용해 웹페이지로 전송되는 정보를 조절하는 것도 중요합니다.

## 참고자료

* [curl 공식 문서](https://curl.se/docs/)
* [HTTP 요청 메서드에 대한 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Methods)
* [HTTP 헤더에 대한 자세한 정보](https://developer.mozilla.org/ko/docs/Web/HTTP/Headers)

## 참고

이 문서는 Bash의 현재 버전인 5.1 기준으로 작성되었습니다. 이외의 다른 버전에서는 결과가 다를 수 있습니다.