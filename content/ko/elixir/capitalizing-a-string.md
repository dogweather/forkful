---
title:                "문자열 대문자화"
aliases:
- ko/elixir/capitalizing-a-string.md
date:                  2024-02-03T19:04:51.766310-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 첫 글자를 대문자로 변환하고 나머지 글자들은 소문자로 유지하는 작업을 문자열 대문자화라고 합니다. 이 작업은 사용자 입력을 형식화하거나 사용자 인터페이스에 텍스트를 표시할 때 일관성과 가독성이 중요하기 때문에 흔히 필요합니다.

## 방법:

Elixir는 내장 함수를 사용하여 별도의 제3자 라이브러리 없이도 문자열을 대문자화하는 간단한 방법을 제공합니다. 다음은 간단한 예입니다:

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

출력:

```
Elixir programming
```

보다 복잡한 대문자화 로직이나 더 많은 제어가 필요한 경우에는, 다양한 String 함수를 조합할 수 있습니다. 예를 들어, 문장의 모든 단어를 대문자화하고 싶다면, 문장을 단어로 나누고, 각각을 대문자화한 후 다시 합칠 수 있습니다:

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

출력:

```
Elixir Is Fun
```

Elixir의 표준 라이브러리가 대부분의 요구를 충족시키지만, 더 미묘한 텍스트 조작을 포함한 고급 문자열 대문자화가 필요한 경우에는 국제화를 위한 Cldr와 같은 제3자 라이브러리를 탐색할 수 있습니다. 이러한 라이브러리는 지역 특정 대문자화 동작을 제공할 수 있습니다.
