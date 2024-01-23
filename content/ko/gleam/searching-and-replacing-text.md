---
title:                "텍스트 검색 및 교체"
date:                  2024-01-20T17:57:47.033002-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 검색 및 교체"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
텍스트 검색 및 교체는 문자열 내에서 특정 단어나 패턴을 찾아 다른 것으로 바꾸는 작업입니다. 프로그래머는 코드 정리, 데이터 변환, 자동화된 수정을 위해 이 기능을 자주 사용합니다.

## How to: (방법)
Gleam에서 문자열 검색 및 교체를 간단하게 할 수 있는 예제입니다. 코드가 짧아서 좋죠.

```gleam
import gleam/string

pub fn main() {
  let text = "Hello, World! Programming is fun in Gleam."
  let new_text = string.replace(text, "Hello", "안녕하세요")
  new_text
  |> string.replace("fun", "재밌어요")
  |> string.replace("Gleam", "글리암")
  |> io.debug
}

// 출력: 안녕하세요, World! Programming is 재밌어요 in 글리암.
```

## Deep Dive (심층 분석)
예전에는 정규 표현식이나 특수 라이브러리 없이 문자열을 검색하고 교체하는 것이 복잡했습니다. Gleam은 Rust처럼 엄격한 타입 시스템을 사용하며, Erlang의 BEAM 가상 머신 위에서 실행되는 함수형 언어입니다. 간단한 기능이지만, 텍스트 처리를 위한 강력한 도구가 됩니다.

Gleam의 `string.replace` 함수는 Rust의 `str.replace`나 Python의 `str.replace`와 같이 간단히 작동합니다. 성능이 중요한 경우, 내부적으로는 더 효율적인 로우-레벨 알고리즘을 사용할 수도 있습니다. 

다른 옵션으로는 `regex` 라이브러리가 있어 복잡한 패턴 매칭이 필요할 때 사용할 수 있습니다. 하지만 대부분의 일반적인 사용 사례에는 기본 제공 함수가 충분합니다.

## See Also (더보기)
- Gleam 공식 문서: https://gleam.run
- String 모듈 문서: https://hexdocs.pm/gleam_stdlib/gleam/string/
- Regex 라이브러리: https://github.com/gleam-lang/regex
- BEAM 가상 머신 정보: https://www.erlang.org/docs
- Rust 프로그래밍 언어: https://www.rust-lang.org/
