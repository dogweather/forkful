---
title:                "문자열의 길이 찾기"
html_title:           "Elixir: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Elixir에서 문자열의 길이를 찾는 것은 무엇이고 왜 개발자들이 이를 하는지에 대해 알아보겠습니다.

## What & Why?

문자열의 길이란 문자열이 포함하고 있는 문자의 개수를 의미합니다. 예를 들어, "Hello World" 문자열의 길이는 11입니다. 이것은 전자우편 주소를 검증하거나 데이터베이스 문자열의 길이를 제한하는 등 다양한 상황에서 유용합니다.

## How to:

```Elixir
str = "Hello World"
IO.puts String.length(str)
```

출력 결과는 다음과 같습니다:

```
11
```

## Deep Dive:

문자열의 길이를 찾는 것은 수많은 프로그래밍 언어에서 지원하는 기능입니다. Elixir에서는 String.length 함수를 사용하여 문자열의 길이를 찾을 수 있습니다. 또한, regex를 사용하여 문자열에서 유니코드 문자의 개수를 나타낼 수도 있습니다.

## See Also:

- String.length 문서: https://hexdocs.pm/elixir/String.html#length/1
- Elixir regex 문서: https://hexdocs.pm/elixir/Regex.html