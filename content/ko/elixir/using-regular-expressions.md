---
title:                "정규 표현식 활용하기"
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇과 왜?)
정규 표현식은 문자열에서 특정 패턴을 찾고 조작하기 위한 강력한 도구입니다. 프로그래머들은 데이터 유효성 검사, 문자열 검색 및 변환 작업을 자동화하기 위해 정규 표현식을 사용합니다.

## How to:
(어떻게 하나요?)
Elixir에서 정규 표현식을 사용하려면 `Regex` 모듈을 활용하세요. 아래 예시를 확인해보세요:

```elixir
# 정규 표현식으로 이메일 주소 찾기
email_pattern = ~r/[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}/
emails = "contact@example.com, not-an-email, admin@test.co.kr"
matches = Regex.scan(email_pattern, emails)
IO.inspect(matches) # [["contact@example.com"], ["admin@test.co.kr"]]
```

```elixir
# 문자열 교체하기
hello_pattern = ~r/\bhello\b/
greetings = "hello world, hello elixir"
new_greetings = Regex.replace(hello_pattern, greetings, "hi")
IO.puts(new_greetings) # "hi world, hi elixir"
```

## Deep Dive:
(심층 탐구)
정규 표현식은 1950년대 매튜키-McNaughton 이론과 자동 상태 머신으로 거슬러 올라갑니다. Elixir에서는 Erlang의 정규 표현식 엔진을 사용합니다. `String` 모듈 함수들과 패턴 매칭을 사용한 간단한 검색 및 문자열 처리가 가능하지만, 복잡한 규칙을 다룰 때는 정규 표현식이 더 효과적일 수 있습니다.

## See Also:
(참조하기)
- Elixir 공식 문서 [Regex 모듈](https://hexdocs.pm/elixir/Regex.html)
- [Rubular: 정규 표현식 테스트 도구](http://rubular.com/)
