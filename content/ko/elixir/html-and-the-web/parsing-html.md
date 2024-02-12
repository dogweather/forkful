---
title:                "HTML 파싱"
aliases: - /ko/elixir/parsing-html.md
date:                  2024-02-03T19:12:00.166293-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Elixir에서 HTML 파싱은 HTML 문서로부터 정보를 추출하는 것을 포함합니다. 프로그래머들은 웹 페이지와 프로그래마틱하게 상호작용하거나, 데이터를 스크레이핑하거나, 웹 상호작용을 자동화하기 위해 이 작업을 수행합니다. 이를 통해 애플리케이션이 웹 콘텐츠를 동적으로 이해하고 활용할 수 있게 됩니다.

## 방법:

Elixir는 견고한 동시성 모델과 함수형 프로그래밍 패러다임을 갖고 있지만, 내장 HTML 파싱 기능은 포함하고 있지 않습니다. 하지만, `Floki`와 같은 인기 있는 서드파티 라이브러리를 사용할 수 있습니다. Floki는 Elixir의 패턴 매칭과 파이핑 특성을 활용하여 HTML 파싱을 직관적이고 효율적으로 만듭니다.

먼저, mix.exs 의존성에 Floki를 추가합니다:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

그런 다음, `mix deps.get`을 실행하여 새로운 의존성을 설치합니다.

이제, 간단한 HTML 문자열을 파싱하여 데이터를 추출해 보겠습니다. `<h1>` 태그 안의 제목을 찾아봅시다:

```elixir
html_content = """
<html>
  <body>
    <h1>안녕하세요, Elixir!</h1>
    <h1>다른 제목</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**샘플 출력:**

```elixir
["안녕하세요, Elixir!", "다른 제목"]
```

더 깊이 파고들어, 링크(`<a>` 태그)와 그 href 속성을 함께 추출하고 싶다면, 다음과 같이 할 수 있습니다:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Elixir 공식 웹사이트</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**샘플 출력:**

```elixir
[{"Elixir 공식 웹사이트", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

이 접근 방식을 통해 HTML 문서를 효율적으로 탐색하고 파싱할 수 있어, Elixir 애플리케이션에서 웹 데이터 추출과 조작 작업을 직관적으로 수행할 수 있습니다.
