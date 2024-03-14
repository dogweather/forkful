---
date: 2024-01-20 17:59:21.959952-07:00
description: "HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC6F9 \uC11C\
  \uBC84\uC5D0 \uC815\uBCF4\uB97C \uC694\uCCAD\uD558\uAC70\uB098 \uC804\uC1A1\uD558\
  \uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uB370\uC774\uD130\uB97C \uC8FC\uACE0\uBC1B\uAC70\uB098, \uC6F9 API\uC640 \uC0C1\
  \uD638 \uC791\uC6A9\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.717581-06:00'
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC6F9 \uC11C\uBC84\
  \uC5D0 \uC815\uBCF4\uB97C \uC694\uCCAD\uD558\uAC70\uB098 \uC804\uC1A1\uD558\uB294\
  \ \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\
  \uC774\uD130\uB97C \uC8FC\uACE0\uBC1B\uAC70\uB098, \uC6F9 API\uC640 \uC0C1\uD638\
  \ \uC791\uC6A9\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
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
