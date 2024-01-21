---
title:                "HTTP 요청 보내기"
date:                  2024-01-20T17:59:49.611627-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청을 보내는 것은 웹 서버에 정보를 요청하거나 데이터를 전송하는 방법입니다. 프로그래머들은 데이터를 교환하고 API를 사용하여 다른 서비스와 상호작용하기 위해 이 방식을 사용합니다.

## How to: (어떻게 하나요?)
```gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn request_example() {
  // HTTP GET 요청을 보내고 결과 처리하기
  let response = httpc.send(http.Request(method: Get, url: "http://example.com"))
  should.equal(response.status, 200)
  should.equal(response.body, "Hello, Gleam!")
}

```

예상 출력:
```
Ok(Response(200, "Hello, Gleam!"))
```

## Deep Dive (깊은 탐색)
HTTP 요청을 보낼 때는 간단한 일부터 복잡한 작업까지 다양한 목적으로 사용됩니다. 처음에는 HTML 페이지나 파일을 요청하기 위해 만들어진 매커니즘이었지만, 지금은 API 통신, 데이터 전송 등에 폭넓게 사용됩니다. Gleam에서는 `gleam/http` 라이브러리를 사용해서 HTTP 요청을 처리할 수 있습니다. 다른 언어와 마찬가지로, 요청 메소드, 헤더, 본문 등을 설정하며 사용할 수 있습니다. 또 다른 대안으로는 `hyper`, `reqwest` 등의 Rust 기반 라이브러리를 FFI (Foreign Function Interface)를 통해 사용할 수도 있습니다.

## See Also (더 보기)
- RESTful API 디자인 가이드: [RESTful API Design](https://restfulapi.net/)