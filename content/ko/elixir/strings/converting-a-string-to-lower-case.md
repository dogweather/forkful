---
title:                "문자열을 소문자로 변환하기"
aliases: - /ko/elixir/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:29.462365-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

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
