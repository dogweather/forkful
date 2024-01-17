---
title:                "표준 오류에 쓰는 것"
html_title:           "Gleam: 표준 오류에 쓰는 것"
simple_title:         "표준 오류에 쓰는 것"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Gleam: 표준 에러에 쓰기

## What & Why?

표준 에러에 쓰기란 무엇일까요? 프로그래머들이 이 작업을 왜 할까요? 표준 에러는 프로그램 실행 중에 발생하는 오류 메시지를 출력하는 데 사용되는 채널입니다. 예기치 않은 상황이 발생할 때, 에러 메시지는 프로그래머들에게 문제 해결에 도움을 주는 중요한 정보가 됩니다.

## How to:

```Gleam
let message = "Hello, world!"
io.error.write(message)
```

위의 코드를 실행하면, 콘솔에 "Hello, world!"라는 에러 메시지가 출력됩니다. 이것은 우리가 의도한 것이 아니었지만, 에러 메시지를 출력하는 방법은 이와 같습니다.

## Deep Dive:

표준 에러에 쓰기는 오래된 통신 방식 중 하나입니다. 이 방식은 프로그램이 오래된 시스템이나 텍스트 기반 인터페이스를 이용해 작동하는 경우에도 유용합니다. 하지만 최근에는 더 진보된 방식인 로깅 시스템을 사용하는 것이 더 효율적입니다. 또한, 표준 에러에 쓰기는 프로그램에서 심각한 오류를 처리할 때만 사용되어야 합니다. 미세한 오류 메시지는 로깅 시스템을 통해 다루는 것이 좋습니다. 이를 따르지 않으면 프로그램의 성능이 저하될 수 있습니다.

## See Also:

- [Gleam 공식 문서](https://gleam.run/documentation/)
- [프로그래밍 관련 블로그](https://medium.com/topic/programming)
- [표준 에러에 쓰기 예제 코드](https://github.com/gleam-lang/gleam/blob/master/examples/io/error.gleam)