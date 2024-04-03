---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:00.166293-07:00
description: "Elixir\uC5D0\uC11C HTML \uD30C\uC2F1\uC740 HTML \uBB38\uC11C\uB85C\uBD80\
  \uD130 \uC815\uBCF4\uB97C \uCD94\uCD9C\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC6F9 \uD398\uC774\uC9C0\
  \uC640 \uD504\uB85C\uADF8\uB798\uB9C8\uD2F1\uD558\uAC8C \uC0C1\uD638\uC791\uC6A9\
  \uD558\uAC70\uB098, \uB370\uC774\uD130\uB97C \uC2A4\uD06C\uB808\uC774\uD551\uD558\
  \uAC70\uB098, \uC6F9 \uC0C1\uD638\uC791\uC6A9\uC744 \uC790\uB3D9\uD654\uD558\uAE30\
  \ \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4. \uC774\uB97C\
  \ \uD1B5\uD574 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC774 \uC6F9 \uCF58\uD150\uCE20\
  \uB97C \uB3D9\uC801\uC73C\uB85C\u2026"
lastmod: '2024-03-13T22:44:54.719029-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uC5D0\uC11C HTML \uD30C\uC2F1\uC740 HTML \uBB38\uC11C\uB85C\uBD80\
  \uD130 \uC815\uBCF4\uB97C \uCD94\uCD9C\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4."
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
