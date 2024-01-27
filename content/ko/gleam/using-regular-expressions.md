---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
정규 표현식(Regular expressions)은 문자열의 패턴을 찾기 위해 사용됩니다. 프로그래머들은 데이터 검증, 검색, 텍스트 가공 등 여러 상황에서 효과적으로 사용합니다.

## How to: (어떻게 사용할까?)
```gleam
import gleam/regex

pub fn run() {
  let pattern = "^The"
  let text = "The quick brown fox jumps over the lazy dog"
  case regex.run(pattern, text) {
    Ok(matches) -> io.println(matches)
    Error(_error) -> io.println("No matches found")
  }
}
```
예제 출력:
```
["The"]
```

## Deep Dive (심화 탐구)
정규 표현식은 1950년대 후반에 수학자 스티븐 클리니에 의해 처음 소개되었습니다. 대안으로 문자열 함수나 파싱 라이브러리를 사용할 수 있지만, 정규 표현식은 강력하고 다목적입니다. Gleam에서는 `gleam/regex` 라이브러리를 통해 엔진을 구현하며, Erlang의 레겍스와 호환됩니다.

## See Also (추가 정보)
- [Regular Expressions Quick Start Guide](https://www.regular-expressions.info/quickstart.html)
- [Stack Overflow Regular Expressions Questions](https://stackoverflow.com/questions/tagged/regex)
