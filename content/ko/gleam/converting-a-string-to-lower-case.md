---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 소문자로 문자열 변환하기: 잠수하고, 탐색하고, 배워보자!

## 무엇이고 왜 그렇게 하는가?
소문자로 문자열 변환은 대문자로 구성된 문자열을 소문자로 바꾸는 프로그래밍 작업을 말합니다. 이는 대소문자 구분 없이 텍스트를 일관되게 비교하고 처리하려는 프로그래머들에게 유용합니다.

## 어떻게 하는가:
Gleam 프로그래밍에서, String 모듈의 `to_lower` 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다.

```gleam
import gleam/string

fn lowercased_string() {
  string.to_lower("HELLO, WORLD!")
}
```

이 코드를 실행하면, 모든 문자열 "HELLO, WORLD!"가 소문자로 변환되어, 출력 결과는 "hello, world!"가 됩니다.

## 깊게 파보기
이전 프로그래밍 언어에서는 소문자 변환을 직접 구현해야 했지만, modern languages like Gleam에서는 미리 제공되는 `to_lower` 함수를 사용하여 이러한 작업을 단순화했습니다.

이 함수는 각 문자를 해당하는 소문자 코드 포인트로 매핑함으로써 동작합니다. 이 방법은 효율적이며 다양한 언어와 문자셋을 지원하게 해줍니다.

물론, 필요하다면 사용자 정의 변환 함수를 작성할 수도 있습니다. 하지만 대부분의 경우 표준 라이브러리를 사용하는 것이 더 효율적일 것입니다.

## 참고 자료
- 다른 문자열 변환 작업에 대해서는 [Unicode Transformation Formats](http://unicode.org/reports/tr15/)를 참고하세요.
- 전반적인 문자열 처리에 대한 연구를 원한다면 [Practical Unicode](http://unicodebook.readthedocs.io/)가 좋은 출발점이 될 것입니다.