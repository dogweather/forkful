---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:51.766310-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\uC9C0 \uAE00\uC790\uB4E4\uC740 \uC18C\
  \uBB38\uC790\uB85C \uC720\uC9C0\uD558\uB294 \uC791\uC5C5\uC744 \uBB38\uC790\uC5F4\
  \ \uB300\uBB38\uC790\uD654\uB77C\uACE0 \uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740\
  \ \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uD615\uC2DD\uD654\uD558\uAC70\uB098 \uC0AC\
  \uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uC5D0 \uD14D\uC2A4\uD2B8\uB97C \uD45C\
  \uC2DC\uD560 \uB54C \uC77C\uAD00\uC131\uACFC \uAC00\uB3C5\uC131\uC774 \uC911\uC694\
  \uD558\uAE30 \uB54C\uBB38\uC5D0 \uD754\uD788 \uD544\uC694\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.698254-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uACE0 \uB098\uBA38\uC9C0 \uAE00\uC790\uB4E4\uC740 \uC18C\uBB38\
  \uC790\uB85C \uC720\uC9C0\uD558\uB294 \uC791\uC5C5\uC744 \uBB38\uC790\uC5F4 \uB300\
  \uBB38\uC790\uD654\uB77C\uACE0 \uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uC0AC\
  \uC6A9\uC790 \uC785\uB825\uC744 \uD615\uC2DD\uD654\uD558\uAC70\uB098 \uC0AC\uC6A9\
  \uC790 \uC778\uD130\uD398\uC774\uC2A4\uC5D0 \uD14D\uC2A4\uD2B8\uB97C \uD45C\uC2DC\
  \uD560 \uB54C \uC77C\uAD00\uC131\uACFC \uAC00\uB3C5\uC131\uC774 \uC911\uC694\uD558\
  \uAE30 \uB54C\uBB38\uC5D0 \uD754\uD788 \uD544\uC694\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

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
