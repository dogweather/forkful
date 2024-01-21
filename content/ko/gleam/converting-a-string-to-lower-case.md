---
title:                "문자열을 소문자로 변환하기"
date:                  2024-01-20T17:38:25.934519-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 바꾸는 것은 문자의 대소문자를 일관되게 만드는 과정입니다. 프로그래머들은 데이터를 비교하거나 정리할 때 이 방법을 사용합니다.

## How to: (어떻게:)
```gleam
import gleam/string

fn main() {
  let my_string = "Gleam is Pretty COOL"
  let lower_string = string.lowercase(my_string)
  io.println(lower_string) // "gleam is pretty cool"
}
```

## Deep Dive (심층 분석)
문자열을 소문자로 바꾸는 것은 프로그래밍의 오랜 문제입니다. 다양한 프로그래밍 언어는 각각의 표준 라이브러리에서 이 기능을 제공합니다. Gleam에서는 `string.lowercase`함수를 사용하여 이 작업을 수행할 수 있습니다. 이 기능의 내재적 복잡성은 유니코드와 문자 인코딩의 다양성 때문에 생깁니다. 유니코드 문자를 올바르게 소문자로 변환하려면 언어와 문자의 특수규칙을 고려해야 합니다. 예를 들어, 터키어에서는 대문자 'I'이 소문자 'i'로 바뀌지 않고 'ı'로 바뀝니다. Gleam의 구현은 이런 복잡성을 추상화하여 사용자에게 간단한 인터페이스를 제공합니다.

## See Also (참조)
- [Unicode Case Mapping Information](http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)
- [The Official Gleam Programming Language Website](https://gleam.run/)