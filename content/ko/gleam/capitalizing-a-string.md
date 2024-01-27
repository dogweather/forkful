---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

문자열을 대문자로 만들기는 각 글자를 대문자 형태로 변환하는 것입니다. 데이터를 표준화하거나 사용자 인터페이스를 개선하는 등의 목적으로 프로그래머들이 사용합니다.

## How to: (어떻게 하나요?)

Gleam에서는 표준 라이브러리의 함수를 사용하여 문자열을 대문자로 만들 수 있습니다. 아래 예제를 확인해보세요.

```gleam
import gleam/string

fn main() {
  let text = "hello, gleam!"
  let uppercase_text = string.uppercase(text)
  uppercase_text
}

// 출력: "HELLO, GLEAM!"
```

## Deep Dive (깊이있게 알아보기)

문자열을 대문자로 바꾸는 기능은 오래전부터 프로그래밍 언어에 포함되어 왔습니다. 간단한 기능이지만, 각 언어에 따라 내부 구현은 상이할 수 있습니다. 예를 들어, 다른 언어에서는 `toUpperCase`, `upper`, 혹은 `MakeUpperCase`와 같은 이름으로 비슷한 기능을 제공합니다.

Gleam에서는 `gleam/string` 모듈 안에 `uppercase` 함수를 통해 이 기능을 제공합니다. 이 함수는 UTF-8 인코딩된 문자열을 받아 처리하므로, 대부분의 문자셋에 대응할 수 있습니다.

유의할 점은 특정 언어에서는 문자 변환 규칙이 일반적인 ASCII 문자와 다를 수 있다는 것입니다. 그러나 Gleam은 유니코드 표준을 잘 따르기 때문에 대부분의 경우에 잘 작동합니다.

## See Also (참고 자료)

- 유니코드와 문자 인코딩에 대한 더 깊은 이해를 위해서는 [유니코드 공식 홈페이지](http://unicode.org)를 방문하세요.
- 다른 언어에서의 문자열 대문자 변환 함수와의 비교를 위해서는 [Rosetta Code](http://rosettacode.org/wiki/Category:String_manipulation)에서 관련 정보를 찾아볼 수 있습니다.
