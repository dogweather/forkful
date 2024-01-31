---
title:                "HTML 파싱"
date:                  2024-01-20T15:31:07.686568-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"

category:             "Elixir"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
HTML 파싱은 HTML 문서를 분석해서 데이터를 추출하거나 의미를 이해하는 과정입니다. 프로그래머들이 데이터를 크롤링하거나, 웹 내용을 분석하고 조작하기 위해 이 작업을 수행합니다.

## How to:
Elixir에서 HTML을 파싱하기 위해 `Floki` 라이브러리를 사용하는 간단한 예제입니다:

```elixir
# 의존성 추가해야 함 (mix.exs 파일에서):
defp deps do
  [
    {:floki, "~> 0.26.0"}
  ]
end

# HTML 문서를 파싱하는 예제
defmodule HtmlParser do
  def parse(html) do
    {:ok, document} = Floki.parse_document(html)
    document |> Floki.find("h1") |> Enum.map(&Floki.text/1)
  end
end

# 사용 예제
html_content = "<html><body><h1>Welcome to Elixir</h1></body></html>"
titles = HtmlParser.parse(html_content)
IO.inspect(titles)  # 출력: ["Welcome to Elixir"]
```
이 코드는 HTML 문서에서 `<h1>` 태그의 텍스트를 추출합니다.

## Deep Dive (심층 정보):
HTML 파싱은 1990년대 초 월드 와이드 웹이 시작되면서 함께 태어났습니다. Elixir에서는 `Floki`, `MochiWeb`, `HTML5ever` 등과 같은 다양한 라이브러리를 사용할 수 있습니다. `Floki`는 jQuery의 선택자와 비슷한 문법을 사용하여 HTML을 쉽게 조회하고 조작할 수 있게 해 줍니다. 내부적으로, `Floki`는 Elixir가 사용하는 `Erlang` 가상 머신 위에서 효율적으로 실행되도록 최적화되어 있습니다.

HTML 파싱의 대안으로는 정규 표현식을 사용하는 방법도 있지만, 이는 복잡하고 실수하기 쉬운 경향이 있습니다. 따라서 안정적이고 정확하게 HTML을 파싱하기 위해서는 `Floki` 같은 전용 라이브러리를 사용하는 것이 좋습니다.

## See Also (참고 자료):
- Floki GitHub repository: [https://github.com/philss/floki](https://github.com/philss/floki)
- Elixir 공식 문서: [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
- HTML5ever, an HTML parser in Rust: [https://github.com/servo/html5ever](https://github.com/servo/html5ever)
- MochiWeb, an Erlang library for building lightweight HTTP servers: [https://github.com/mochi/mochiweb](https://github.com/mochi/mochiweb)
