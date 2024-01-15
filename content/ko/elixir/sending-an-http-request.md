---
title:                "HTTP 요청 보내기"
html_title:           "Elixir: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 왜
HTTP 요청을 보내는 이유는 다양합니다. 대표적으로는 API와의 상호 작용을 통해 데이터를 가져오는 것이나 웹 페이지를 업데이트하는 것, 서버로부터 파일을 다운로드 받는 것 등이 있습니다.

# 어떻게
Elixir에서 HTTP 요청을 보내는 것은 간단한데, `HTTPoison` 라이브러리를 사용하면 됩니다. 예를 들어, `HTTPoison.get/2` 함수를 사용하여 GET 요청을 보낼 수 있습니다.

```Elixir
response = HTTPoison.get("https://example.com")
```

요청을 보낸 후에는 `response` 변수에 응답이 담겨있게 됩니다. 이를 통해 다양한 정보를 가져올 수 있습니다. 예를 들어, 상태 코드는 `response.status_code`를 통해 확인할 수 있고, 응답 바디는 `response.body`를 통해 가져올 수 있습니다.

# 깊이 들어가기
HTTP 요청을 보내는 과정에서 발생할 수 있는 다양한 이슈들을 다루어보겠습니다. 예를 들어, `HTTPoison`은 세션과 리다이렉트를 자동으로 관리해주기 때문에 개발자는 이를 개별적으로 신경 쓰지 않아도 됩니다. 또한, 인증이 필요한 요청을 보낼 때에는 `HTTPoison`의 인증 옵션을 활용할 수 있습니다.

# 더 알아보기
HTTP 요청을 위해 사용할 수 있는 다른 라이브러리들도 있습니다. 예를 들어, `Mint`는 Elixir 네트워킹 클라이언트 라이브러리로서 매우 빠른 성능을 자랑합니다. 또한, `Tesla`는 쉽게 커스터마이징이 가능한 클라이언트 라이브러리입니다.

# 참고하기
- [HTTPoison 라이브러리](https://github.com/edgurgel/httpoison)
- [Mint 라이브러리](https://github.com/elixir-mint/mint)
- [Tesla 라이브러리](https://github.com/teamon/tesla)