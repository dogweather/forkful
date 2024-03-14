---
date: 2024-01-26 03:39:03.849892-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD55C\uB2E4\uB294 \uAC83\uC740 \uADF8 \uC548\uC758 \uAE68\uB057\uD55C \uD14D\uC2A4\
  \uD2B8\uB97C \uC5BB\uAE30 \uC704\uD574 \uC774\uB7EC\uD55C \uCD94\uAC00 \uB798\uD37C\
  \uB97C \uBC97\uACA8\uB0B4\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825\uC744 \uC815\uD654\uD558\uACE0\
  , \uC624\uB958\uB97C \uD53C\uD558\uACE0, \uB530\uC634\uD45C\uAC00 \uAE30\uB2A5\uC774\
  \ \uC544\uB2CC \uBC29\uD574\uAC00 \uB418\uB294 \uCC98\uB9AC\uB97C \uC704\uD574 \uB370\
  \uC774\uD130\uB97C \uC900\uBE44\uD558\uAE30 \uC704\uD574\uC11C \uC774 \uC791\uC5C5\
  \uC744 \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.704770-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uADF8 \uC548\uC758 \uAE68\uB057\uD55C \uD14D\uC2A4\uD2B8\
  \uB97C \uC5BB\uAE30 \uC704\uD574 \uC774\uB7EC\uD55C \uCD94\uAC00 \uB798\uD37C\uB97C\
  \ \uBC97\uACA8\uB0B4\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825\uC744 \uC815\uD654\uD558\uACE0, \uC624\
  \uB958\uB97C \uD53C\uD558\uACE0, \uB530\uC634\uD45C\uAC00 \uAE30\uB2A5\uC774 \uC544\
  \uB2CC \uBC29\uD574\uAC00 \uB418\uB294 \uCC98\uB9AC\uB97C \uC704\uD574 \uB370\uC774\
  \uD130\uB97C \uC900\uBE44\uD558\uAE30 \uC704\uD574\uC11C \uC774 \uC791\uC5C5\uC744\
  \ \uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 따옴표를 제거한다는 것은 그 안의 깨끗한 텍스트를 얻기 위해 이러한 추가 래퍼를 벗겨내는 것을 의미합니다. 프로그래머들은 입력을 정화하고, 오류를 피하고, 따옴표가 기능이 아닌 방해가 되는 처리를 위해 데이터를 준비하기 위해서 이 작업을 합니다.

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
