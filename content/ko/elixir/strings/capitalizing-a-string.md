---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:51.766310-07:00
description: "\uBC29\uBC95: Elixir\uB294 \uB0B4\uC7A5 \uD568\uC218\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uBCC4\uB3C4\uC758 \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC \uC5C6\
  \uC774\uB3C4 \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\uB294 \uAC04\
  \uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740\
  \ \uAC04\uB2E8\uD55C \uC608\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.698254-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uB294 \uB0B4\uC7A5 \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBCC4\
  \uB3C4\uC758 \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC \uC5C6\uC774\uB3C4 \uBB38\
  \uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\
  \uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

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
