---
date: 2024-01-26 03:39:03.849892-07:00
description: "\uBC29\uBC95: Elixir\uC5D0\uB294 \uB0B4\uC7A5\uB41C '\uB530\uC634\uD45C\
  \ \uC81C\uAC70' \uAE30\uB2A5\uC774 \uC5C6\uC9C0\uB9CC, \uD328\uD134 \uB9E4\uCE6D\
  \uC774\uB098 `String` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC790\uC2E0\uB9CC\
  \uC758 \uBC29\uBC95\uC744 \uC27D\uAC8C \uAD6C\uD604\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4. \uB2E4\uC74C \uCF54\uB4DC \uC2A4\uB2C8\uD3AB\uC744 \uCC38\uC870\uD558\uC138\
  \uC694."
lastmod: '2024-03-13T22:44:54.704770-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uC5D0\uB294 \uB0B4\uC7A5\uB41C '\uB530\uC634\uD45C \uC81C\uAC70'\
  \ \uAE30\uB2A5\uC774 \uC5C6\uC9C0\uB9CC, \uD328\uD134 \uB9E4\uCE6D\uC774\uB098 `String`\
  \ \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC790\uC2E0\uB9CC\uC758 \uBC29\uBC95\
  \uC744 \uC27D\uAC8C \uAD6C\uD604\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 방법:
Elixir에는 내장된 '따옴표 제거' 기능이 없지만, 패턴 매칭이나 `String` 함수를 사용하여 자신만의 방법을 쉽게 구현할 수 있습니다. 다음 코드 스니펫을 참조하세요:

```elixir
# 패턴 매칭 사용
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# 샘플 사용법
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# String.trim/1 사용
def unquote_string(string), do: String.trim(string, "'\"")

# 샘플 사용법
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

두 방법 모두의 출력:
```
"Hello, World!"
```

## 심층 분석
예전에 문자열 안의 따옴표는 지뢰밭이었습니다—잘못 다루면, 붐, 구문 오류나 보안 구멍이 생겼습니다. Elixir에서 패턴 매칭은 문자열을 레고 블록처럼 다루게 해, 정밀하게 분해하고 재조립할 수 있게 합니다. 또한 강력한 `String` 모듈이 유용하며, `trim` 함수로 유연하게 따옴표를 제거합니다. 대안은 무엇일까요? 정규 표현식은 따옴표를 제거할 수 있으며, 기본 제거 작업 이상이 필요하다면 외부 라이브러리가 추가 기능을 제공할 수 있습니다.

## 참고
더 깊이 탐구하기 위해 이것들을 확인하세요:
- [Elixir의 String 모듈](https://hexdocs.pm/elixir/String.html)
- [Elixir에서 패턴 매칭에 대해 더 알아보기](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Elixir에서의 정규 표현식 (Regex 모듈)](https://hexdocs.pm/elixir/Regex.html)
