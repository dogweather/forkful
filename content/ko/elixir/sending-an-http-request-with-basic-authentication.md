---
title:                "Elixir: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜: HTTP 요청을 기본 인증과 함께 전송해야 하는 이유

HTTP 요청에 기본 인증을 포함시키는 것은 애플리케이션과 서버간의 보안을 강화하기 위해서입니다. 이것은 사용자가 인증되지 않은 요청을 한다거나 중요한 정보를 가져오기 위한 요청을 한다거나 할 때 매우 중요합니다. 또한, 인증된 요청을 통해서 특정 작업에 대한 권한을 필요한 사용자만 가지도록 제어할 수도 있습니다.

## 방법: Elixir에서 HTTP 요청에 기본 인증 추가하기

먼저, HTTP 클라이언트 라이브러리인 `HTTPoison`을 설치해야 합니다. 이 라이브러리는 HTTP 요청을 보내는 기능을 제공하기 때문에 우리가 필요한 기능을 간편하게 구현할 수 있습니다. 먼저, 다음과 같이 `HTTPoison`을 프로젝트에 추가해야 합니다.

```elixir
def deps do
  [
    {:httpoison, "~> 1.6"}
  ]
end
```

다음으로, `HTTPoison`을 사용해서 HTTP 요청을 보내는 예제를 살펴보겠습니다.

```elixir
# HTTPoison 라이브러리 import
import HTTPoison

# HTTP 요청
response = HTTPoison.request(:get, "https://example.com", [], basic_auth: {"username", "password"})

# 응답 코드 확인
response.status_code

# 응답 바디 출력
IO.puts(response.body)

# 응답 헤더 출력
IO.puts(response.headers)
```

위의 예제에서는 `HTTPoison`의 `request` 함수를 사용해서 GET 메소드로 `https://example.com`에 요청을 보낸 후, 응답 코드, 바디, 헤더를 출력하는 예제입니다. `basic_auth` 옵션을 사용해 사용자 이름과 비밀번호를 전달하면, HTTP 요청에 기본 인증을 포함시킬 수 있습니다.

## 딥 다이브: 기본 인증이란?

기본 인증은 애플리케이션과 서버 간의 보안을 강화하기 위해 사용되는 인증 방식 중 하나입니다. 이 방식은 요청을 보낸 사용자가 인증된 사용자인지 확인하기 위해 사용됩니다. 기본 인증은 사용자 이름과 비밀번호를 인코딩해서 `Authorization` 헤더에 포함시켜서 보내는 방식입니다. 이 방식은 암호화되지 않은 텍스트로 이루어져 있기 때문에, 보안 위험이 존재합니다. 따라서 HTTPS와 같은 안전한 프로토콜을 사용하는 것이 좋습니다.

## 참고 문서

- [HTTPoison 라이브러리 문서](https://hexdocs.pm/httpoison/1.6.0/HTTPoison.html)
- [HTTP 기본 인증에 대한 RFC](https://tools.ietf.org/html/rfc7617)
- [Elixir 애플리케이션 개발 스크랩저트](https://programmer.help/blogs/a-simple-way-to-realize-scraper-using-elixir.html)

## 직접 해보세요!

이 글을 읽으셨다면 지금 바로 Elixir로 간단한 HTTP 요청을 보내보세요. `HTTPoison` 라이브러리를 사용해서 간단한 GET 요청을 구현해보고, 기본 인증을 추가해보세요. 그리고 다양한 옵션