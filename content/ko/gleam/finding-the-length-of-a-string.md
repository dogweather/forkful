---
title:                "문자열의 길이 찾기"
date:                  2024-01-20T17:47:36.320596-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열 길이 찾기는 문자열에 있는 문자 수를 알아내는 것입니다. 프로그래머들은 루프, 조건문 처리 또는 잘라내기 위해서 이 정보를 사용합니다.

## How to:
```gleam
import gleam/string

// 문자열 길이 구하기
let greeting = "안녕하세요"
let length = string.len(greeting)
println(length) // 출력: 5
```

## Deep Dive
문자열의 길이를 찾는 작업은 오래된 컴퓨팅 문제입니다. 프로그래밍 언어마다 다르게 구현됩니다. 예를 들어, C에서는 문자열의 끝에 null 값을 사용해서 끝을 나타냅니다. Gleam에서는 `string.len` 함수를 사용하는데, 이것은 문자열 내의 유니코드 스칼라 값의 수를 세어서 길이를 반환합니다. 길이를 구하는 다른 방법으로 문자열을 순환하면서 직접 세는 것도 있지만, Gleam의 표준 함수가 훨씬 간결하고 효율적입니다.

## See Also
- Gleam 공식 문서: [https://gleam.run/book](https://gleam.run/book)
- 문자열 처리에 대한 더 깊은 이해: [https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance](https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance)