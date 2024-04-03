---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:00.166293-07:00
description: "\uBC29\uBC95: Elixir\uB294 \uACAC\uACE0\uD55C \uB3D9\uC2DC\uC131 \uBAA8\
  \uB378\uACFC \uD568\uC218\uD615 \uD504\uB85C\uADF8\uB798\uBC0D \uD328\uB7EC\uB2E4\
  \uC784\uC744 \uAC16\uACE0 \uC788\uC9C0\uB9CC, \uB0B4\uC7A5 HTML \uD30C\uC2F1 \uAE30\
  \uB2A5\uC740 \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uD558\
  \uC9C0\uB9CC, `Floki`\uC640 \uAC19\uC740 \uC778\uAE30 \uC788\uB294 \uC11C\uB4DC\uD30C\
  \uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. Floki\uB294 Elixir\uC758 \uD328\uD134 \uB9E4\uCE6D\uACFC \uD30C\uC774\
  \uD551 \uD2B9\uC131\uC744\u2026"
lastmod: '2024-03-13T22:44:54.719029-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uB294 \uACAC\uACE0\uD55C \uB3D9\uC2DC\uC131 \uBAA8\uB378\uACFC \uD568\
  \uC218\uD615 \uD504\uB85C\uADF8\uB798\uBC0D \uD328\uB7EC\uB2E4\uC784\uC744 \uAC16\
  \uACE0 \uC788\uC9C0\uB9CC, \uB0B4\uC7A5 HTML \uD30C\uC2F1 \uAE30\uB2A5\uC740 \uD3EC\
  \uD568\uD558\uACE0 \uC788\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

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
