---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
텍스트 검색과 교체는, 특정 문자열을 찾아 다른 문자열로 대체하는 프로그래밍 기법입니다. 이는 데이터 정제, 탐색, 변환 등 다양한 작업을 수행하기 위해 프로그래머들이 사용합니다.

## 어떻게:
아래에 Gleam 언어로 텍스트를 검색하고 교체하는 방법이 나와 있습니다.

```Gleam
import gleam/string

fn main(args: List(String)) {
  let replaced_string = string.replace("Hello, World!", "World", "Gleam")
  assert("Hello, Gleam!" == replaced_string)
  replaced_string
}
```
위의 코드는 "Hello, World!" 문자열에서 "World"를 "Gleam"으로 교체합니다. 따라서 결과는 "Hello, Gleam!"가 됩니다.

## 깊이 들여다보기
텍스트 검색 및 교체는 컴퓨터 과학의 초기 단계부터 존재했습니다. 원시 코드에 존재하는 특정 문자열을 변경하려는 프로그래머의 요구로부터 시작되었습니다. 상기 코드 예제에서는 기본 string.replace 함수를 사용했습니다. 이는 간단한 예제에서는 효과적이지만, 정규식을 사용하여 더 복잡한 패턴을 검색하거나 교체하는 경우도 많습니다. Gleam에서는 이를 위해 [gleam/regex](https://hexdocs.pm/gleam_stdlib/gleam/regex/) 라이브러리를 사용할 수 있습니다.