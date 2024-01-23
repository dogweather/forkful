---
title:                "문자열 연결하기"
date:                  2024-01-20T17:35:05.179735-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 연결(concatenation)은 두 개 이상의 문자열을 하나로 붙이는 것입니다. 프로그래머들은 데이터를 합치거나, 사용자화된 메시지를 만들거나, 파일 경로를 조합할 때 사용합니다.

## How to (어떻게 하나요?)
Elixir에서 문자열을 연결하려면 `<>` 연산자를 사용합니다. 간단하죠. 직접 해볼까요?

```elixir
# 두 문자열 연결하기
name = "세계"
greeting = "안녕, " <> name <> "!"
IO.puts greeting
```

실행 결과는 이렇습니다:

```
안녕, 세계!
```

변수 없이도 하실 수 있고요:

```elixir
# 바로 문자열 연결하기
IO.puts "커피" <> "와 " <> "코드"
```

실행 결과:

```
커피와 코드
```

## Deep Dive (더 깊이 파기)
연결은 Elixir에서 효율적입니다. 문자열은 바이너리로 표현되기 때문이죠. 이 방식은 BEAM 머신과 어우러져 효율을 높입니다.

역사적으로, 다른 언어에서 '+'나 ' . ' 같은 연산자들이 문자열을 합치는 데 사용됐어요. 하지만 Elixir에서는 `<>` 가 그 일을 합니다.

밑줄 치며, Elixir에는 문자열 템플릿(Interpolation)이라는 것도 있습니다. `"#{변수}"` 식으로 사용하죠. 큰 따옴표 안에서 `#{}`를 사용하여 변수나 표현식 결과를 문자열에 집어넣을 수 있습니다.

예시:

```elixir
name = "세계"
greeting = "안녕, #{name}!"
IO.puts greeting
```

실행 결과는 위와 같겠죠.

## See Also (더 알아보기)
- Elixir 공식 문서의 [String module](https://hexdocs.pm/elixir/String.html)
- [Programming Elixir](https://pragprog.com/titles/elixir16/programming-elixir-1-6/) - Dave Thomas의 책
- [Elixir School](https://elixirschool.com/en/) - 문자열과 더 들어갈 수 있는 좋은 자료입니다.
