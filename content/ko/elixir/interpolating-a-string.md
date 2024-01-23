---
title:                "문자열 보간하기"
date:                  2024-01-20T17:50:32.841408-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 보간하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
문자열 내삽은 변수나 표현식의 값을 문자열에 집어넣는 것입니다. 값이 변할 때마다 자동으로 문자열을 갱신하기 위해 사용합니다.

## How to: (어떻게 하나요?)
```Elixir
# 변수에 기반한 문자열 내삽
name = "세상"
greeting = "안녕, #{name}!"
IO.puts greeting
# 출력: 안녕, 세상!

# 수식을 사용한 문자열 내삽
hours = 12
message = "지금은 #{hours * 2}시입니다."
IO.puts message
# 출력: 지금은 24시입니다.
```

## Deep Dive (깊이 있는 정보)
Elixir에서 문자열 내삽은 내부적으로 `String.Chars` 프로토콜을 사용합니다. 이것은 Ruby의 영향을 받았으며, Elixir의 매크로 기능을 활용해 컴파일 타임에 문자열과 관련된 값을 템플릿 안으로 삽입합니다. 더 간단한 방법으로 `<>` 연산자를 사용해 문자열을 연결할 수 있지만, 내삽은 사용하기 편하고 읽기 쉬운 코드를 작성할 수 있도록 도와줍니다.

```Elixir
# `<>` 연산자를 사용한 문자열 연결
name = "세상"
greeting = "안녕, " <> name <> "!"
IO.puts greeting
# 출력: 안녕, 세상!
```

## See Also (더 보기)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/strings/)
- [Elixir Documentation: String](https://hexdocs.pm/elixir/String.html)
- [Elixir Forum for discussions](https://elixirforum.com/)
