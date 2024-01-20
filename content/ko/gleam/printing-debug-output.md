---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디버그 출력이란 프로그래머가 코드가 어떻게 동작하는지 확인하기 위해 출력하는 로그 내용입니다. 이는 프로그램이 예상대로 동작하는지 또는 예상치 못한 문제가 발생했는지를 파악할 수 있게 도움이 됩니다.

## 다음과 같이 해보세요:

```Gleam
import gleam/io

fn main() {
  let x = 1
  io.debug(x)
  x
}
```

이 Gleam 코드 응용 프로그램은 실행 중에 `x`라는 변수에 대한 정보를 출력합니다.


## 심층 탐구

디버그 출력의 이용은 컴퓨터 프로그래밍 초창기로 거슬러 올라갈 수 있습니다. 당시의 프로그래머들은 디버그 출력을 통해 자신의 코드가 정확하게 동작하는지 확인하곤 했습니다.

디버그 출력에 대한 대안으로는 소스 코드에 특수한 `println` 문을 추가하거나 전용 디버깅 도구를 사용할 수 있습니다. 그러나 이러한 방법 모두 추가적인 코드 작성이나 설정 과정이 필요하며 때로는 문제의 원인을 찾는데 더 복잡해질 수 있습니다.

디버그 출력은 간단히 `io.debug` 함수를 사용하여 간단히 구현할 수 있습니다. 필요한 변수나 표현식을 함수의 인수로 전달하면 됩니다. 실질적으로 이는 프로그램의 표준 오류 스트림에 메시지를 출력합니다.

## 참고 자료

디버그에 대한 좀 더 자세한 내용은 다음의 문서를 참고해보세요.

2. [디버깅 기술에 대한 아티클](https://medium.com/@alchemist.unicorn/a-guide-to-debugging-d8bd4237fdfb)