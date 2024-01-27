---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
문자열의 첫 글자를 대문자로 바꾸는 것을 말합니다. 주로 문장을 시작할 때나 제목, 이름 등을 명확하게 구분하기 위해 사용합니다.

## How to (어떻게 하나요?)
Elixir에서 문자열의 첫 글자를 대문자로 바꾸기 위해서는 `String.capitalize/1` 함수를 사용합니다.

```elixir
IO.puts String.capitalize("elixir")
IO.puts String.capitalize("처음 시작하는")
```

예상 결과:

```
Elixir
처음 시작하는
```

## Deep Dive (심층 분석)
Elixir에서의 문자열 대문자화는 `String.capitalize/1` 함수로 처리됩니다. 이 함수는 문자열의 첫 글자를 대문자로, 나머지는 소문자로 변환합니다. 역사적으로는 각 프로그래밍 언어마다 이런 기능을 제공하는데, Elixir는 매우 간단하고 이해하기 쉬운 방법을 제공합니다. 비슷한 기능으로는 `upcase`와 `downcase`가 있으며 각각 문자열을 전부 대문자나 소문자로 변환합니다. 구현면에서는 유니코드 문자열 처리를 위해 Elixir는 내부적으로 `String.Normalize`를 사용합니다.

## See Also (관련 링크)
- Elixir 공식 문서 [`String.capitalize/1`](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [`String.upcase/1`](https://hexdocs.pm/elixir/String.html#upcase/1)
- [`String.Normalize`](https://hexdocs.pm/elixir/String.html#normalize/2)
