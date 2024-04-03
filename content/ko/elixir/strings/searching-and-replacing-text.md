---
date: 2024-01-20 17:57:56.570894-07:00
description: "How to: Elixir\uC5D0\uC11C\uB294 `String` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\
  \uD574 \uC27D\uAC8C \uD14D\uC2A4\uD2B8\uB97C \uAC80\uC0C9\uD558\uACE0 \uAD50\uCCB4\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798 \uC608\uC81C\uB97C \uCC38\uACE0\
  \uD558\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.700938-06:00'
model: gpt-4-1106-preview
summary: "Elixir\uC5D0\uC11C\uB294 `String` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD574\
  \ \uC27D\uAC8C \uD14D\uC2A4\uD2B8\uB97C \uAC80\uC0C9\uD558\uACE0 \uAD50\uCCB4\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## How to:
Elixir에서는 `String` 모듈을 사용해 쉽게 텍스트를 검색하고 교체할 수 있습니다. 아래 예제를 참고하세요.

```elixir
# 문자열에서 "world"를 찾아 "Elixir"로 바꿉니다.
original_text = "Hello, world!"
replaced_text = String.replace(original_text, "world", "Elixir")

IO.puts replaced_text
```

샘플 출력:

```
Hello, Elixir!
```

정규 표현식을 사용하려면, `Regex` 모듈을 이용할 수 있습니다.

```elixir
# 문자열에서 숫자를 찾아 괄호로 둘러싸기
text_with_numbers = "My 2 cats and 3 dogs."
regex_pattern = ~r/\d/

new_text = Regex.replace(regex_pattern, text_with_numbers, fn number -> "{#{number}}" end)

IO.puts new_text
```

샘플 출력:

```
My {2} cats and {3} dogs.
```

## Deep Dive
엘릭서(Elixir)의 텍스트 검색 및 교체 기능은 내부적으로 Erlang의 문자열 처리 기능을 사용합니다. 이는 Elixir가 에를랑 VM(가상 머신) 위에서 실행되기 때문입니다. 

전통적으로 문자열을 처리하는 다른 방법은 문자열을 순회하며 교체하는 것이지만, 이는 느릴 수 있습니다. Elixir는 바이너리 패턴 매칭을 사용하여 효율적으로 문자열을 처리합니다.

제공되는 `String` 모듈에서는 원하는 패턴을 검색하고 그 결과를 다양한 방법으로 교체하는 여러 함수를 제공합니다. `String.replace/3`는 가장 기본적인 함수이며, 정규 표현식을 사용할 경우 `Regex.replace/4`를 사용할 수 있습니다.

## See Also
- Elixir 공식 문서의 `String` 모듈: https://hexdocs.pm/elixir/String.html
- Elixir 공식 문서의 `Regex` 모듈: https://hexdocs.pm/elixir/Regex.html
- 문자열 패턴 매칭에 관한 가이드: https://elixir-lang.org/getting-started/pattern-matching.html
