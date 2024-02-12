---
title:                "웹 페이지 다운로드하기"
aliases:
- ko/elixir/downloading-a-web-page.md
date:                  2024-01-20T17:43:47.147827-07:00
model:                 gpt-4-1106-preview
simple_title:         "웹 페이지 다운로드하기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
웹 페이지 다운로드는 인터넷에서 HTML 문서를 로컬 장치에 가져오는 과정입니다. 프로그래머들은 데이터 수집, 웹 스크레이핑 또는 백업을 위해 이 작업을 할 때가 있습니다.

## How to: (어떻게 하나요?)
Elixir에서는 HTTPoison이나 HTTPotion 같은 라이브러리를 사용해 웹 페이지를 다운로드할 수 있습니다. 예제를 따라 해 봅시다.

```elixir
# HTTPoison 라이브러리를 추가합니다:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# 페이지를 다운로드하는 함수:
def download_page(url) do
  case HTTPoison.get(url) do
    {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
      {:ok, body}
    {:ok, %HTTPoison.Response{status_code: status_code}} ->
      {:error, "Error with status code: #{status_code}"}
    {:error, %HTTPoison.Error{reason: reason}} ->
      {:error, reason}
  end
end

# 함수 예제 사용:
download_page("https://hex.pm")
```

## Deep Dive: (깊이 있게 알아보기)
웹 페이지 다운로드는 최초의 웹 상호작용 중 하나였습니다. Elixir에서 이 작업은 Erlang의 강력한 OTP 플랫폼 위에 구축된 컨커런시(concurrency)와 결합하여 효율적으로 수행됩니다. HTTPoison은 hackney 라이브러리 위에 구축된 HTTP 클라이언트입니다. HTTPotion이라는 대안도 있지만, 더 이상 활발하게 관리되지 않습니다.

다운로드 프로세스에는 HTTP 프로토콜의 GET 요청을 사용하는 것이 포함됩니다. 이것은 서버에 웹 페이지의 사본을 요청하는 가장 간단한 방법입니다. 여러분의 요청은 HTTP 헤더, 쿠키, 사용자 에이전트 등 다양한 HTTP 요청 패러미터를 포함할 수 있습니다.

## See Also: (관련 정보 보기)
- [HTTPoison documentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [hackney on GitHub](https://github.com/benoitc/hackney)
- [Erlang OTP](https://www.erlang.org/doc/design_principles/users_guide.html)
- [Hex.pm](https://hex.pm/) (Elixir 라이브러리를 찾아보기)
