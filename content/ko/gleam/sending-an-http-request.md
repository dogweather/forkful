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

## Gleam로 HTTP 요청을 보내보자! 😎

## 무엇인가요? 그리고 왜 그렇게 하나요?

HTTP 요청을 보내는 것은 서버와 클라이언트 사이에서 데이터를 교환하기 위한 프로토콜입니다. 프로그래머들은 HTTP 요청을 보내는 이유는 웹에서 데이터를 가져오고, 서버와 상호 작용하며, 다른 프로그램과 통신하기 위해서입니다.

## 어떻게 하나요?

아래와 같이 Gleam 코드 블록을 사용하여 HTTP 요청을 보낼 수 있습니다. 👇

```Gleam
use http
use json

let url = "https://api.example.com/users/1"

let response =
  http.get(url)
  |> case response {
      Ok(r) -> r.body |> json.decode_map
      Err(_) -> panic("Failed to send HTTP request!")
    }

IO.inspect(response)
```

위 코드는 `https://api.example.com/users/1` URL에서 GET 요청을 보내고, 서버로부터 받은 데이터를 JSON으로 디코드한 후에 `response` 변수에 저장합니다. 그리고 `IO.inspect` 함수를 사용하여 결과 데이터를 콘솔에 출력합니다.

## 더 들어가보기

### 역사적 배경

HTTP는 웹 프로그래밍에서 가장 중요한 프로토콜 중 하나입니다. Tim Berners-Lee가 1989년에 처음 제안하여, 1991년에 최초의 웹 브라우저 가운데 하나인 WorldWideWeb에서 구현되었습니다.

### 대안들

HTTP 요청을 보내는 가장 일반적인 방법은 `httpc`, `hackney` 혹은 `elli` 등의 Erlang 라이브러리를 사용하는 것입니다. 하지만 이러한 라이브러리들은 단일 프로세스에서 실행되는 Erlang 애플리케이션에 적합하지 않을 수 있습니다. 이를 방지하기 위해, Gleam에는 `http` 모듈이 포함되어 있어, Erlang 라이브러리보다 좀 더 쉽게 HTTP 요청을 보낼 수 있습니다.

### 구현 세부사항

HTTP 요청을 보내려면, 네트워크로 데이터를 전송할 수 있는 기능을 가진 라이브러리가 필요합니다. Gleam은 언어 자체에 네트워크를 위한 기능들이 탑재되어 있지 않기 때문에, HTTP 요청을 보내려면 Erlang 라이브러리를 사용해야 합니다.

## 이외의 자료

- [Gleam 공식 문서](https://gleam.run/documentation/)
- [Erlang HTTP client 라이브러리](https://github.com/davisp/http)
- [Erlang 웹 서버 라이브러리](https://github.com/elixir-lang/plug)