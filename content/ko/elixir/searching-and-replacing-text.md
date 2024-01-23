---
title:                "텍스트 검색 및 교체"
date:                  2024-01-20T17:57:56.570894-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 검색 및 교체"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
텍스트 검색 및 교체는 문자열 내에서 특정 패턴을 찾아 다른 텍스트로 바꾸는 작업입니다. 프로그래머들은 주로 코드 리팩토링, 데이터 정제 또는 자동화된 편집을 할 때 이 작업을 수행합니다.

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
