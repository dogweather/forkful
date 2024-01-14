---
title:                "Elixir: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜
이 블로그 포스트에서는 Elixir로 HTTP 요청을 보내는 방법을 소개합니다. Elixir로 HTTP 요청을 보내는 것은 웹 개발에서 매우 중요한 요소입니다. 이것은 서버와 클라이언트 사이에 정보를 교환하기 위해 사용되며, 웹 애플리케이션 개발에서 필수적이기 때문입니다.

## 방법
우선, `HTTPotion`이라는 패키지를 설치해야 합니다. 그리고 나서 `HTTPotion.get` 함수를 사용하여 GET 요청을 보낼 수 있습니다. 아래는 예제 코드와 그에 따른 샘플 출력입니다.
```Elixir
# 패키지 설치
mix deps.get

# 라이브러리 임포트
require HTTPotion

# GET 요청 보내기
HTTPotion.get("https://www.example.com")
```

```
# 샘플 출력
{200,
 [{"date", "Tue, 12 Oct 2021 07:00:00 GMT"},
  {"expires", "-1"},
  {"cache-control", "private, max-age=0"},
  {"x-content-type-options", "nosniff"},
  {"server", "gws"},
  {"x-xss-protection", "0"},
  {"alternate-protocol", "443:quic=xxxxx:quic=xxxxx:quic=xxxxx:quic=xxxxx"},
  {"accept-ranges", "none"},
  {"connection", "close"}], "Hello, World!"}
```

## 깊이 있는 내용
HTTP 요청은 `GET`, `POST`, `PUT`, `DELETE`와 같은 다양한 메서드를 사용하여 보낼 수 있습니다. 또한, 쿼리 스트링이나 헤더를 추가할 수도 있습니다. `HTTPotion` 패키지는 이러한 기능을 모두 제공하며, 더 자세한 내용은 공식 문서를 참고하시기 바랍니다.

## See Also
- [HTTPotion 공식 문서](https://hexdocs.pm/httpotion/api-reference.html)
- [HTTP 요청 메서드에 대한 자세한 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Methods)
- [HTTP 요청 헤더에 대한 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Headers)