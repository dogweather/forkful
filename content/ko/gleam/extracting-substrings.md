---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:45:36.455251-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇과 왜?)
문자열에서 부분 문자열을 추출하는 것은 그 문자열의 일부를 선택하는 작업입니다. 이는 데이터 검증, 파싱 또는 형식 변환 등의 작업에서 필수적입니다.

## How to:
(방법)
```Gleam
import gleam/string

pub fn main() {
  let text = "안녕하세요, 프로그래밍 세계!"
  let greeting = string.slice(text, 0, 5) // "안녕하세요"
  let identity = string.slice(text, 10, 15) // "프로그래밍"

  // 콘솔에 출력합니다.
  io.debug(greeting)
  io.debug(identity)
}

// 출력:
// "안녕하세요"
// "프로그래밍"
```

## Deep Dive
(심층 분석)
문자열에서 부분 문자열을 추출하는 기능은 문자열을 다루는 대부분의 프로그래밍 언어에 포함되어 있습니다. Gleam의 경우 안정성과 명료성을 중요시하는 함수를 제공합니다. `string.slice` 함수는 문자열의 시작 인덱스와 끝 인덱스를 지정하여 부분 문자열을 추출합니다. 각 문자를 개별 유닛으로 다루는 대신 바이트 오프셋을 사용하면 안전하지 못한 작업이 될 수 있는데, 이는 특히 UTF-8 인코딩과 같이 다양한 바이트 길이를 갖는 문자 인코딩에서 중요합니다. Gleam은 이러한 복잡성을 추상화해서 안전한 문자열 처리를 도와줍니다.

## See Also
(참조)
- Gleam's `string` module documentation: https://hexdocs.pm/gleam_stdlib/gleam/string/
- Unicode string handling best practices: https://www.unicode.org/standard/principles.html
- Rust's string slice safety discussions, which influenced language designs like Gleam's: https://doc.rust-lang.org/book/ch08-02-strings.html
