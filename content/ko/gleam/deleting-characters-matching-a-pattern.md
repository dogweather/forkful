---
title:                "패턴에 일치하는 문자 삭제"
date:                  2024-01-20T17:42:09.033669-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자 패턴에 맞는 문자를 삭제하는 건 특정 형태의 데이터를 제거하는 작업입니다. 프로그래머들은 불필요하거나 민감한 정보를 없애거나, 데이터를 깨끗하게 정리할 때 이 기능을 사용합니다.

## How to: (방법)
Gleam 프로그래밍에서 패턴에 맞는 문자 삭제는 주로 정규 표현식을 활용합니다. 예시 코드를 보면서 학습해봅시다.

```gleam
import gleam/regex

pub fn delete_pattern(text: String, pattern: String) -> Result(String, Nil) {
  let re = regex.compile(pattern)
  case re {
    Ok(regex) -> Ok(regex.replace(text, ""))
    Error(_) -> Error(Nil)
  }
}

pub fn main() {
  let text = "Sensitive info: 123-45-6789, remove ASAP!"
  let pattern = "\\d{3}-\\d{2}-\\d{4}" // This is a pattern for a social security number

  case delete_pattern(text, pattern) {
    Ok(cleaned_text) -> println(cleaned_text) // Outputs: "Sensitive info: , remove ASAP!"
    Error(_) -> println("Failed to delete pattern.")
  }
}
```

## Deep Dive (심층 탐구)
역사적으로 문자 패턴 삭제는 데이터 처리와 텍스트 변환에서 중요한 역할을 해왔습니다. 처음엔 소프트웨어가 단순 문서 내 문자열을 찾고 바꾸는 수준이었지만, 지금은 정규 표현식을 통해 보다 복잡한 패턴 인식과 수정이 가능합니다. 대안으로는 문자열 함수나 파싱 라이브러리 등이 있지만, 정규 표현식은 간단하고 유연합니다.

Gleam에서는 `gleam/regex` 모듈을 사용하여 정규 표현식을 컴파일하고 문자열에서 패턴을 찾고 삭제할 수 있습니다. 구현에 있어 내부적으로는 Erlang의 정규 표현식 라이브러리를 활용합니다. 이는 Gleam의 BEAM 가상 머신 기반 인프라에 잘 연동되어 강력한 성능을 제공합니다.

## See Also (참조)
- A regular expression guide: [Regular-Expressions.info](https://www.regular-expressions.info/)
- Erlang Regex module: [Erlang Regex](http://erlang.org/doc/man/re.html)
