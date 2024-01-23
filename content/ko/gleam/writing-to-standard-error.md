---
title:                "표준 오류로 쓰기"
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 사용하는가?)
표준 오류는 오류 메세지와 로그 같은 정보를 출력하기 위한 특별한 통로입니다. 프로그래머는 시스템이 제대로 동작하지 않을 때 유용한 정보를 제공하기 위해 이를 사용합니다.

## How to: (어떻게 사용하나요?)
Gleam을 사용해 표준 오류에 쓰는 예시는 아래와 같습니다:

```gleam
import gleam/io

pub fn main() {
  io.stderr("에러 발생: 파일을 찾을 수 없습니다.")
}
```

실행 결과:
```
에러 발생: 파일을 찾을 수 없습니다.
```

## Deep Dive (심층 정보)
과거에는 표준 출력(stdout)에 모든 정보를 출력했지만, 오류 메세지와 일반 출력을 분리함으로써 디버깅이 더 쉬워졌습니다. 표준 오류(stderr)의 대안들로 로깅 라이브러리나 시스템의 로그 서비스 등이 있지만 이들은 설정이 복잡할 수 있습니다. Gleam은 Erlang의 VM을 사용하여 구현되어 있기 때문에, 표준 오류 또한 Erlang의 I/O 기능을 기반으로 합니다.

## See Also (참고 자료)
- Gleam 공식 문서: [Gleam documentation](https://gleam.run)
- Erlang의 표준 오류 처리에 대한 정보: [Erlang -- Errors](http://erlang.org/doc/apps/stdlib/io_protocol.html)
- 프로그래밍에서의 로깅에 대한 베스트 프랙티스: [Logging Best Practices](https://12factor.net/logs)
