---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 길이를 찾는 것은 문자열에 포함된 문자의 총 개수를 구하는 것입니다. 이 정보는 텍스트 처리, 입력 검증 등 프로그래밍 작업에서 중요한 역할을 합니다.

## 방법:

Gleam에서 문자열의 길이를 구하는 것은 간단합니다. 아래의 코드를 확인해보세요.

```Gleam
import gleam/string

fn main() {
  let my_string = "안녕하세요"
  let length = string.len(my_string)
    
  length
  |> Int.to_string
  |> io.println
}
```
출력:
```Gleam
5
```
위 예제에서는 "안녕하세요"의 길이인 5가 출력됩니다.

## 깊게 알아보기

문자열의 길이를 찾는 것은 프로그래밍의 초기부터 매우 중요한 작업이었습니다. C언어에서는 문자열의 끝에 NULL 문자(`\0`)가 있어 이를 이용해 문자열의 길이를 구했습니다. Gleam에서는 String 모듈의 `len` 함수를 이용해 문자열의 길이를 구합니다.

그외에도 문자열을 순회하면서 길이를 계산한는 방법, 리슨티프 형태를 이용하는 방법 등으로 표현하고 구현할 수도 있습니다. 그러나 Gleam에서는 내장 함수를 이용하는 것이 더 간단하고 효율적입니다.

## 참고하기

다른 관련된 소스는 아래에서 확인하세요:

1. [Gleam Documentation - String Module](https://gleam.run/documentation/guide/#string)
2. [String Functions - Elixir School](https://elixirschool.com/en/lessons/basics/strings/)
3. [Gleam Language: A Type-Safe Language for the Beam VM](https://increment.com/programming-languages/gleam-language-a-type-safe-language-for-the-beam-vm/)