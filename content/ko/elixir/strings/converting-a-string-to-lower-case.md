---
date: 2024-01-20 17:38:29.462365-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.703321-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to: (어떻게 하나요?)
```elixir
# 문자열을 소문자로 변환하는 예제:
str = "Hello, World!"
lowercase_str = String.downcase(str)
IO.puts(lowercase_str)
```
출력 결과:
```
hello, world!
```

## Deep Dive (심층 분석)
Elixir에서 문자열을 소문자로 바꾸기 위해 `String.downcase/1` 함수를 사용합니다. 과거, 문자열 처리는 프로그래밍 언어와 인코딩에 따라 다양한 방식으로 진행되었습니다. 현재 Elixir는 유니코드 호환성을 위해 UTF-8을 사용하고, 언어의 표준 라이브러리에서 제공하는 이 함수를 통하여 안정적으로 문자열을 소문자로 변환할 수 있습니다. 대안으로 `String.downcase/2`를 써서 특정 로케일을 기반으로 변환할 수도 있습니다. Elixir의 `String.downcase/1` 함수는 내부적으로 Erlang의 :unicode 모듈을 사용해 유니코드 Case Mapping 규칙에 따라 문자열을 소문자로 변환합니다.

## See Also (함께 보기)
- [String module documentation](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
- [Elixir School - Strings](https://elixirschool.com/en/lessons/basics/strings/)
