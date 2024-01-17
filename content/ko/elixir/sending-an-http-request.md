---
title:                "Http 요청 보내기"
html_title:           "Elixir: Http 요청 보내기"
simple_title:         "Http 요청 보내기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
HTTP 요청을 보내는 것은 서버와 통신하고 데이터를 요청하거나 보내는 것을 의미합니다. 프로그래머는 이것을 함으로써 서버와 웹 애플리케이션 사이에서 데이터를 주고받을 수 있게 됩니다.

## 하는 법:
```Elixir 
url = "https://example.com/api/user/1"
response = HTTPoison.get(url, [])
IO.puts(response.body)
```
위의 예시 코드는 HTTPoison 라이브러리를 사용하여 주어진 URL에서 GET 요청을 보내고, 서버의 응답을 출력하는 방법을 보여줍니다.

## 깊이 들어가기:
HTTP 요청은 현재 웹 개발에서 필수적인 요소이며, 브라우저를 통한 모든 웹 기능은 HTTP 요청을 통해 이뤄집니다. Elixir에서는 HTTPoison 외에도 다양한 라이브러리를 사용할 수 있으며, 각각의 라이브러리마다 장단점이 있습니다. HTTP 요청을 구현하는 과정에서는 HTTP 메서드, 헤더, 바디 등 다양한 요소들을 고려해야 합니다.

## 관련 자료:
- Elixir 공식 문서: https://elixir-lang.org/getting-started/mix-otp/http-client.html
- HTTPoison 라이브러리: https://hexdocs.pm/httpoison/HTTPoison.html