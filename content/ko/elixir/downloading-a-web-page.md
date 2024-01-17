---
title:                "웹 페이지 다운로드하기"
html_title:           "Elixir: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 뭐 & 왜?

웹 페이지를 다운로드한다는 것은 간단히 말해 인터넷에서 파일을 내 컴퓨터로 내려받는 것을 뜻합니다. 프로그래머들은 이것을 하는 이유는 다양합니다. 예를 들어, 웹 스크래핑과 같은 작업을 하기 위해서이거나, 서버에서 웹 페이지를 제공하는 것과 같은 자동화된 작업을 수행하기 위해서입니다.

# 어떻게:

```Elixir
# 요청 라이브러리 불러오기
require HTTPoison

# HTTP 요청 보내기
HTTPoison.get!("https://www.example.com")

# 가져온 결과 확인하기
{:ok, %HTTPoison.Response{status_code: 200, body: "<html><head>...</head><body>...</body></html>"}}
```

# 깊이 들어가보기:

이러한 웹 페이지 다운로드 작업은 인터넷에서 정보를 얻고 처리하는 데 필수적입니다. 이전에 이 작업을 수행하기 위해 프로그래머들은 더 낮은 수준의 프로그래밍 언어를 사용해야 했지만, 지금은 Elixir와 같은 고급 언어를 사용할 수 있습니다. 또한 라이브러리를 사용하는 것 또한 이 작업을 간단하게 만드는 방법 중 하나입니다.

# 관련 자료:

- Elixir 공식 홈페이지 (https://elixir-lang.org)
- HTTPoison 라이브러리 문서 (https://hexdocs.pm/httpoison/HTTPoison.html)