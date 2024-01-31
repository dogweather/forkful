---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:01:37.018214-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"

category:             "Elixir"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
HTTP 요청에 기본 인증(Basic Authentication)을 추가하여 서버에 보냅니다. 보안이 필요한 데이터에 접근할 때 인증 정보를 전달하기 위해 사용합니다.

## How to: (어떻게 하나요?)
Elixir에서 HTTP 요청을 보내려면 `HTTPoison` 같은 라이브러리가 유용합니다. 기본 인증을 사용하는 예시를 보여 드리겠습니다.

```elixir
# HTTPoison 라이브러리를 프로젝트에 추가하세요.
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

def fetch_with_basic_auth(url, username, password) do
  # Basic 인증 헤더를 생성합니다.
  auth_header = "Basic " <> Base.encode64("#{username}:#{password}")
  headers = [{"Authorization", auth_header}]

  # HTTP GET 요청을 보냅니다.
  HTTPoison.get(url, headers)
end

# 함수를 사용해 봅시다.
case fetch_with_basic_auth("https://some-protected-resource.com", "user123", "password!") do
  {:ok, response} -> IO.inspect(response.body)
  {:error, error} -> IO.inspect(error)
end
```

예상 출력은 보안된 리소스의 내용이거나, 오류 메시지 일 것입니다.

## Deep Dive (깊게 들여다보기)
HTTP 기본 인증은 HTTP 헤더를 통해 유저네임과 패스워드를 인코딩된 형태로 전송합니다. 1990년대 초 HTTP 프로토콜의 일부로 만들어졌으며, 여전히 간단한 인증 수단으로 사용됩니다. 하지만 인코딩은 암호화가 아닙니다; 보안이 중요할 때는 HTTPS와 같은 보안 채널을 사용해야 합니다.

기본 인증의 대안으로는 OAuth, API 키, JWT(Json Web Tokens) 등이 있습니다. 이들은 더 안전하거나 사용하기에 더 편리한 방법을 제공합니다.

`HTTPoison` 라이브러리 외에도 `Tesla`나 `Hackney` 같은 라이브러리를 사용할 수 있습니다. `HTTPoison`은 내부적으로 `Hackney`를 이용합니다. `Tesla`는 매크로를 이용해 클라이언트를 더 쉽게 구성할 수 있는 기능을 제공합니다.

## See Also (더 보기)
- HTTPoison documentation: https://hexdocs.pm/httpoison/
- Basic Authentication on MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Tesla Elixir library: https://github.com/teamon/tesla
- Elixir School's lesson on HTTPoison: https://elixirschool.com/en/lessons/libraries/httpoison/
