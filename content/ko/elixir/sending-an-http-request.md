---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

HTTP 요청 전송은 서버로 정보를 공유하거나 서버에서 정보를 얻기 위한 방법입니다. 개발자가 이를 사용하여 웹 서비스와 상호 작용합니다.

## 사용 방법:
Elixir에서는 보통 `:httpc.request` 함수를 사용하여 HTTP 요청을 전송합니다. 여기서는 GET 요청을 어떻게 보내는지 간단하게 보여드릴게요:

```elixir
{:ok, {{'HTTP/1.1', 200, 'OK'}, headers, body}} = :httpc.request('http://example.com')
IO.inspect(body)

# output
"<html>...</html>"
```

POST 요청도 쉽게 보낼 수 있습니다:

```elixir
{:ok, {{'HTTP/1.1', 200, 'OK'}, headers, body}} =
  :httpc.request(:post, {'http://example.com', [], 'application/x-www-form-urlencoded', 'key1=value1&key2=value2'})

IO.inspect(body)

# output
"{\"key1\":\"value1\",\"key2\":\"value2\"}"
```

## 깊은 이해

- **역사적 맥락**: HTTP 요청은 1991년에 처음 소개된 이후로 웹의 핵심 역할을 해왔습니다. Elixir는 Erlang 표준 라이브러리(HTTP client)를 채택하여 HTTP 요청을 처리합니다.

- **대안**: Elixir에는 HTTPoison과 같은 다른 HTTP 클라이언트 라이브러리도 있습니다. HTTPoison은 간단하고 직관적인 API를 제공합니다.

- **구현 세부 정보**: `:httpc.request` 함수를 호출할 때, `:httpc` 모듈은 주어진 매개변수를 바탕으로 HTTP 요청을 생성하고 요청을 보냅니다. 응답이 오면 함수는 `{:ok, response}` 형식의 튜플을 반환합니다.

## 참고 자료
- Elixir 공식 문서: [http://elixir-lang.org/docs.html](http://elixir-lang.org/docs.html)
- Erlang `:httpc` Module 문서: [https://erlang.org/doc/man/httpc.html](https://erlang.org/doc/man/httpc.html)
- 보다 강력한 HTTP 클라이언트 라이브러리, HTTPoison: [https://hexdocs.pm/httpoison/readme.html](https://hexdocs.pm/httpoison/readme.html)