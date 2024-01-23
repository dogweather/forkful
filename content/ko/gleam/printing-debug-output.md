---
title:                "디버그 출력을 찍어보기"
date:                  2024-01-20T17:52:39.157722-07:00
model:                 gpt-4-1106-preview
simple_title:         "디버그 출력을 찍어보기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
디버그 출력은 코드가 의도한 동작을 하는지 확인하기 위해 중간 결과나 상태를 화면에 찍어보는 것입니다. 프로그래머들은 버그를 찾고, 알고리즘이 올바르게 작동하는지 검증하기 위해 사용합니다.

## How to: (방법)
```gleam
import gleam/io

pub fn main() {
  let debug_output = "Debug info: value is 42"
  io.debug(debug_output)
}
```
샘플 출력:
```
Debug info: value is 42
```
코딩하면서 'io.debug' 함수로 중간 값을 출력해 볼 수 있습니다. 로직 확인이 필요할 때 유용합니다.

## Deep Dive (심화 탐구)
디버그 출력은 개발 초기에 특히 중요합니다. 시스템이 복잡해질수록 디버그 로그를 통해 문제를 쉽게 찾아낼 수 있죠. 역사적으로는 printf 디버깅이라 불리우며, 대부분의 프로그래밍 언어에서 비슷한 기능을 제공합니다.

Gleam에서는 `io.debug` 함수를 통해 구현합니다. 이 함수는 표준 오류(stderr)로 메시지를 출력합니다. 이는 일반적인 `io.println` 함수가 표준 출력(stdout)으로 메시지를 보내는 것과 대비됩니다.

디버그 vs. 로깅: 디버그 출력은 개발 중에 진단을 위해 사용하는 반면, 로깅은 운영 중인 애플리케이션의 상태를 기록합니다. Gleam은 로깅을 위한 별도의 라이브러리나 프레임워크를 사용할 수 있습니다.

## See Also (참고 자료)
- [Gleam Documentation](https://gleam.run/book/)
