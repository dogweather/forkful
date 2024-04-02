---
date: 2024-01-20 17:38:29.462365-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294\
  \ \uAC83\uC740 \uB300\uBB38\uC790\uAC00 \uD3EC\uD568\uB41C \uBB38\uC790\uC5F4\uC758\
  \ \uBAA8\uB4E0 \uBB38\uC790\uB97C \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC77C\uAD00\
  \uC131\uC744 \uC720\uC9C0\uD558\uACE0, \uB300\uC18C\uBB38\uC790 \uAD6C\uBD84 \uC5C6\
  \uC774 \uBB38\uC790\uC5F4\uC744 \uBE44\uAD50\uD558\uAC70\uB098 \uBD84\uB958\uD560\
  \ \uB54C \uC774 \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.703321-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uAC83\
  \uC740 \uB300\uBB38\uC790\uAC00 \uD3EC\uD568\uB41C \uBB38\uC790\uC5F4\uC758 \uBAA8\
  \uB4E0 \uBB38\uC790\uB97C \uC18C\uBB38\uC790\uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC77C\uAD00\uC131\
  \uC744 \uC720\uC9C0\uD558\uACE0, \uB300\uC18C\uBB38\uC790 \uAD6C\uBD84 \uC5C6\uC774\
  \ \uBB38\uC790\uC5F4\uC744 \uBE44\uAD50\uD558\uAC70\uB098 \uBD84\uB958\uD560 \uB54C\
  \ \uC774 \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## What & Why? (무엇 그리고 왜?)
문자열을 소문자로 바꾸는 것은 대문자가 포함된 문자열의 모든 문자를 소문자로 바꾸는 과정입니다. 프로그래머들은 일관성을 유지하고, 대소문자 구분 없이 문자열을 비교하거나 분류할 때 이 작업을 합니다.

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
