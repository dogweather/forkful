---
title:                "HTTP 요청 보내기"
date:                  2024-01-20T17:59:21.959952-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
HTTP 요청을 보내는 것은 웹 서버에 정보를 요청하거나 전송하는 방법입니다. 프로그래머들은 데이터를 주고받거나, 웹 API와 상호 작용하기 위해 이를 사용합니다.

## How to:
Elixir에서 HTTP 요청을 보내려면, `HTTPoison` 라이브러리를 사용하는 것이 일반적입니다. `mix.exs` 파일에 의존성을 추가하고, 예시로 간단한 GET 요청을 보내 봅시다.

```elixir
# mix.exs에 dependency 추가
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# iex에서 다음을 실행합니다
HTTPoison.start()

# GET 요청을 보냅니다
response = HTTPoison.get!("https://jsonplaceholder.typicode.com/posts/1")

# 응답 내용을 출력합니다
IO.inspect(response)
```

샘플 출력:

```elixir
%HTTPoison.Response{
  body: "{ ... }",
  headers: [
    {"Content-Type", "application/json; charset=utf-8"},
    ...
  ],
  status_code: 200
}
```

## Deep Dive
HTTP 요청은 인터넷의 핵심을 이루는 행위입니다. 1990년대 초 웹의 출현과 함께 HTTP는 표준 통신 규약으로 자리잡았습니다. `HTTPoison`은 Elixir용 인기 HTTP 클라이언트 라이브러리입니다. Erlang의 `hackney` 라이브러리를 기반으로 하며, Elixir에서 쉽게 HTTP 요청을 보낼 수 있게 해줍니다.

선택 사항으로 `Tesla`나 `HTTPotion`과 같은 다른 라이브러리도 있습니다. 이러한 라이브러리들은 각기 독특한 구문과 기능을 제공합니다. HTTPoison은 요청을 보내고, 응답을 동기적으로 또는 비동기적으로 처리하는 강력한 기능들을 갖추고 있어 실제 개발 작업에서 유연하게 활용할 수 있습니다.

## See Also
- 공식 HTTPoison GitHub 페이지: https://github.com/edgurgel/httpoison
- Elixir에서 다른 HTTP 클라이언트인 Tesla 사용하기: https://github.com/teamon/tesla
- Erlang `hackney` 라이브러리에 대한 자세한 정보: https://github.com/benoitc/hackney
