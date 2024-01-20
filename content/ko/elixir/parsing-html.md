---
title:                "HTML 파싱"
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

HTML 파싱은 HTML 문서를 분석하고 그 내용을 처리하는 것을 의미합니다. 프로그래머는 HTML 파싱을 통해 웹사이트에서 필요한 데이터를 추출하거나, 웹 페이지의 구조를 이해하거나, 콘텐츠를 수정하거나, 새로운 웹 페이지를 생성하는데 사용합니다.

## '어떻게' 부분:

Elixir에서는 Floki 라이브러리를 사용하여 HTML을 파싱할 수 있습니다. 아래는 간단한 예제입니다.

```elixir
defp deps do
  [
    {:floki, "~> 0.30.0"}
  ]
end
```

이 버전의 Floki 라이브러리를 프로젝트에 추가하고, HTML 문서를 파싱하는 함수를 생성합니다.

```elixir
defmodule HtmlParser do
  def parse_html(html) do
    html
    |> Floki.find("p")
    |> Enum.map(fn {_, [], inner_text} -> inner_text end)
  end
end
```

위의 함수는 HTML 문서에서 "p" 태그를 찾아서 텍스트만 추출합니다.

## 깊이있게 보기:

HTML 파싱은 웹 개발 초기부터 필요한 기술 중 하나였습니다. 이는 HTML 문서의 구조를 이해하고 데이터를 추출하는데 필요하기 때문입니다. 다른 대안에는 정규 표현식(regex)이 있지만, HTML 파싱이 복잡한 구조에 대해 더 강한 핸들링을 제공하므로 보다 나은 선택이 될 수 있습니다.

Elixir에서 HTML 파싱은 Floki 라이브러리를 사용합니다. 이 라이브러리는 HTML DOM에 대한 간단한 인터페이스를 제공하며, CSS 선택자를 지원하여 목표된 HTML 요소를 쉽게 찾을 수 있도록 합니다.

## 참고하셔도 좋은 자료:

1. Floki 라이브러리: https://hexdocs.pm/floki/Floki.html
2. Elixir 공식 문서: https://elixir-lang.org/getting-started/introduction.html
3. HTML 파싱에 대한 상세 글: https://www.freecodecamp.org/news/best-practices-in-html-parsing-with-elixir/