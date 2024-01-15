---
title:                "HTTP 요청 보내기"
html_title:           "Gleam: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 왜: HTTP 요청을 보내는 이유

HTTP 요청을 보내는 것은 웹 개발에서 매우 중요한 요소입니다. 이를 통해 웹 애플리케이션과 다른 서버 간에 데이터를 교환하고, API를 사용하여 다른 애플리케이션과 통신할 수 있습니다.

# 방법: HTTP 요청 보내기

Gleam 언어에서 HTTP 요청을 보내는 방법은 매우 간단합니다. 먼저 `gleam/http` 라이브러리를 import합니다. 그리고 `gleam/http` 모듈의 `send` 함수를 사용해 요청을 보냅니다. 이때, `send` 함수의 첫 번째 인자는 요청 메소드(GET, POST 등), 두 번째 인자는 요청을 보낼 URL, 그리고 옵션으로 세번째 인자에는 요청에 필요한 헤더와 바디 데이터를 전달할 수 있습니다.

```
import gleam/http

resp =
  http.send("GET", "https://example.com")
  # 세번째 인자는 옵셔널입니다.
  # 예를 들어, HTTP 헤더를 담기 위해 사용할 수 있습니다.
  ["User-Agent: Gleam/1.0", "Content-Type: application/json"]
  # 바디 데이터는 JSON 형식으로 전달합니다.
  (Ok("{\"name\": \"Gleam\"}"))

# 요청에 대한 응답을 확인합니다.
case resp do
  Ok(response) -> response
  Err(e) -> "Something went wrong"
end
```

위 코드의 실행 결과는 아래와 같습니다.

```
# 성공적인 요청의 경우
status: 200 OK
body: {"name": "Gleam"}

# 요청 실패의 경우
Something went wrong
```

# 깊이 파헤치기: HTTP 요청 보내기

보다 복잡한 통신을 위해서는 `gleam/http/build` 모듈을 사용할 수 있습니다. `build` 모듈은 단순한 텍스트 기반의 요청 대신에 다양한 자료형을 사용하여 요청을 생성할 수 있습니다. 예를 들어, `gleam/encoding` 모듈을 사용해 요청 바디를 생성할 수 있고, `gleam/json` 모듈을 사용해 JSON 형식의 데이터를 쉽게 생성할 수 있습니다.

```
import gleam/http/build
import gleam/encoding
import gleam/json

# 요청 바디를 만듭니다.
body =
  encoding.encode([bytes("\x01\x02")])
  |> json.decoder()

# JSON 형식의 요청 데이터를 만듭니다.
json_data = %Json.Decode
  { name: "Gleam", type: "language" }

# build 모듈을 사용하여 요청을 생성합니다.
req =
  build.request(url="https://example.com", json=json_data)
  |> build.method("POST")
  |> build.header("Content-Type", "application/json")
  |> build.body(body)

# 요청을 보냅니다.
resp = http.send(req)

# 요청에 대한 응답을 확인합니다.
case resp do
  Ok(response) -> response
  Err(e) -> "Something went wrong"
end
```

# 참고하실 만한 것들

- Gleam 언어 공식 문서: https://gleam.run/
- Gleam `http` 라이브러리: https://gleam.run/documentation/stdlib/http
- Gleam `http/build` 모듈: https://gleam.run/documentation/stdlib/http#build