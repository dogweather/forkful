---
title:                "부분 문자열 추출하기"
html_title:           "Elixir: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 꼬리 문자열 추출하기 (Extracting Substrings)

## 무엇을 & 왜? (What & Why?)
문자열에서 일부의 특정 부분만 추출하는 것을 꼬리 문자열 추출이라고 합니다. 프로그래머들은 이 기능을 사용하여 원하는 정보를 쉽게 찾을 수 있고, 처리할 수 있습니다.

## 방법 (How to:)
꼬리 문자열 추출은 간단한 방법으로 수행할 수 있습니다. 예를 들어, 다음과 같이 하면 됩니다.

```Elixir
string = "Hello world!"
substring = String.slice(string, 6..-1)
IO.puts(substring)
```

출력: world!

## 깊이 파고들기 (Deep Dive):
꼬리 문자열 추출은 Elixir에서 부분 문자열을 추출하는 가장 효과적인 방법입니다. 이전의 다른 언어들에서는 부분 문자열을 추출하기 위해 복잡한 코드를 작성해야 했지만, Elixir에서는 간단하게 처리할 수 있습니다. Elixir의 String 모듈에는 다양한 메소드가 있어, 다양한 방법으로 부분 문자열을 추출할 수 있습니다.

## 관련 자료 (See Also):
- Elixir 문서: [String 모듈](https://hexdocs.pm/elixir/String.html)
- 주요 용어 설명: [부분 문자열 추출](https://en.wikipedia.org/wiki/Substring)