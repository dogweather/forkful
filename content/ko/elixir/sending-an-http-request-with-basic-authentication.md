---
title:                "기본 인증과 함께 http 요청 보내기"
html_title:           "Elixir: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
HTTP 요청을 기본 인증과 함께 보내는 것은 프로그래머들이 자주 하는 작업입니다. 이를 통해 애플리케이션이 요청을 보낼 때 인증 정보를 함께 보낼 수 있어서 안전하고 보안이 강화됩니다.

# 어떻게:
```Elixir
HTTPoison.request(:get, "https://example.com/", basic_auth: {"username", "password"})
```
이 코드를 사용하면 Elixir에서 HTTPoison 라이브러리를 활용하여, 기본 인증을 함께 보낸 GET 요청을 보낼 수 있습니다. 출력은 요청을 받은 서버의 HTML 코드를 반환합니다.

# 깊이 파기:
HTTP 기본 인증은 클라이언트가 서버에 요청을 보낼 때 기본적인 사용자 이름과 비밀번호를 함께 보내는 인증 방식입니다. 이 방식은 현재 권장되지 않는 보안 프로토콜 중 하나입니다. 대안으로는 보다 강력한 보안을 제공하는 OAuth 또는 JWT 인증 방식이 있습니다. 구현 방식은 Elixir에서는 HTTPoison 라이브러리를 활용하고 있습니다.

# 더 알아보기:
- [HTTPoison 라이브러리 문서](https://hexdocs.pm/httpoison/)
- [HTTP 기본 인증 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication#Basic_Authentication)
- [보안을 강화하는 다른 인증 방식들](https://blog.restcase.com/4-most-used-rest-api-authentication-methods/)