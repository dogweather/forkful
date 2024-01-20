---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Elixir를 이용한 기본 인증을 통한 HTTP 요청 전송

## 무엇과 왜?

기본 인증을 통한 HTTP 요청 전송은 클라이언트가 서버에 접근하기 위해 사용자 이름과 비밀번호를 전송하는 방법입니다. 이 방법을 사용하면 서버 측인증을 별도의 복잡한 과정 없이 간단하게 처리할 수 있습니다.

## 어떻게?

```Elixir
defmodule HttpClient do
  require HTTPoison

  def fetch(url, username, password) do
    headers = [basic_auth: {"username", "password"}]
    HTTPoison.get!(url, headers, [])
  end
end
```
이 스크립트를 실행하면 다음과 같은 결과를 받을 수 있습니다.

```Elixir
result = HttpClient.fetch("http://example.com", "username", "password")
IO.inspect(result)
```
결과는 HTTP 요청의 응답을 반환합니다.

## 깊이 파보기

기본 인증을 사용한 HTTP 요청은 웹의 초기 시절부터 사용되어 온 기능입니다. HTTP 1.0 및 1.1 명세에 포함되어 있습니다. Bearer Token, Digest Access Authentication 등과 같은 좀 더 복잡하고 보안 강화된 방법이 있지만 아직까지 기본 인증은 그 단순함과 이해하기 쉬움 때문에 널리 사용되고 있습니다.

## 참고하기

- HTTP Basic Authentication: https://en.wikipedia.org/wiki/Basic_access_authentication
- Elixir HTTPoison Documentation: https://hexdocs.pm/httpoison/readme.html
- HTTP 1.0/1.1 Specification: https://tools.ietf.org/html/rfc2616