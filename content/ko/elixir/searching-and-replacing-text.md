---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 대체하는 작업을 하는 이유는 코드 작성 속도를 높이고 효율적인 개발을 돕기 위해서입니다.

## 방법

Elixir는 강력한 검색 및 대체 기능을 제공합니다. `String.replace` 함수를 사용하여 텍스트를 대체할 수 있습니다. 예를 들어, 다음과 같이 작성할 수 있습니다.

```elixir
my_text = "안녕하세요! Elixir로 코딩해봅시다!"
new_text = String.replace(my_text, "안녕하세요", "반가워요")
IO.puts(new_text)
```

출력 결과는 다음과 같습니다.

```elixir
반가워요! Elixir로 코딩해봅시다!
```

## 깊게 파고들기

검색 및 대체 기능은 패턴 매칭을 기반으로 작동합니다. 따라서 간단한 문자열 뿐만 아니라 정규표현식을 사용하여 복잡한 패턴을 정의할 수도 있습니다. 또한 `String.replace` 함수가 아닌 `Regex.replace` 함수를 사용하여 해당 패턴에 대한 원하는 동작을 정의할 수 있습니다.

See Also 
---

- Elixir 공식 문서: https://hexdocs.pm/elixir/String.html#replace/4
- Regular expression 모듈: https://hexdocs.pm/elixir/Regex.html