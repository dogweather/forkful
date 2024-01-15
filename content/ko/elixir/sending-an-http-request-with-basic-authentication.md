---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Elixir: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청을 기본 인증과 함께 보내는 방법에 대해 알아보기 전에, 먼저 왜 이런 행동을 해야하는지 알아봅시다. 기본 인증은 웹 애플리케이션에서 사용자 인증을 처리할 때 매우 일반적으로 사용되는 방법입니다. 따라서 HTTP 요청을 보낼 때 기본 인증을 사용하면 사용자 인증을 간단하게 처리할 수 있습니다.

## 어떻게
기본 인증을 사용하는 HTTP 요청을 보내는 방법은 매우 간단합니다. 먼저 Elixir의 HTTP 클라이언트 라이브러리인 HTTPoison을 설치해야합니다. 이제 코드 블록으로 구문 분석을 시작할 수 있습니다.

```Elixir
# HTTPoison 라이브러리를 가져옵니다.
import HTTPoison

# 요청할 URL과 사용자 자격 증명을 정의합니다.
url = "https://example.com"
user = "username"
password = "password"

# 기본 인증을 사용하는 GET 요청을 보냅니다.
response = HTTPoison.get(url, [], [basic_auth: {user, password}])

# 응답을 콘솔에 출력합니다.
IO.puts response.body

# 결과:
# <html>
#   <body>
#     <h1>Hello, World!</h1>
#   </body>
# </html>
```

기본 인증을 사용하는 요청을 보내기 위해 `HTTPoison.get` 함수를 사용했습니다. 이 함수는 3개의 인수를 받습니다. 첫 번째 인수는 요청할 URL, 두 번째 인수는 요청 매개변수, 세 번째 인수는 옵션입니다. 기본 인증을 사용하기 위해 옵션 인수에 `basic_auth: {user, password}`를 전달했습니다. 이렇게 하면 요청 헤더에 사용자 이름과 비밀번호가 포함된 기본 인증 헤더가 추가됩니다. 따라서 서버에서 요청을 받을 때 이 정보를 사용하여 사용자 인증을 처리할 수 있습니다.

## 깊이 파고들기
HTTP 요청에 대해 더 깊이 이해하기 위해서는 HTTP 클라이언트와 서버 간의 통신 프로토콜에 대해 알아야합니다. HTTP는 Stateless 프로토콜이므로 서버는 각 요청을 개별적으로 처리하고, 이전 요청의 상태를 알지 못합니다. 따라서 기본 인증을 사용하여 각 요청마다 사용자 인증을 처리하는 것이 중요합니다.

여러분은 이러한 기능을 직접 구현하지 않고도 HTTPoison과 같은 라이브러리를 사용하여 기본 인증을 간단하게 처리할 수 있습니다. 이러한 라이브러리는 HTTP 요청을 보낼 때 매우 유용합니다.

## See Also
- [HTTPoison 공식 문서](https://hexdocs.pm/httpoison/1.6.0/HTTPoison.html)
- [Elixir HTTP 클라이언트를 사용하여 API 요청 보내기](https://medium.com/the-ideal-system/using-the-elixir-http-client-to-send-api-requests-dcdd0d6b9c1f)
- [HTTP 클라이언트 및 기본 인증에 대한 보안 주의 사항](https://cheatsheetseries.owasp.org/cheatsheets/HTTP_Strict_Transport_Security_Cheat_Sheet.html)