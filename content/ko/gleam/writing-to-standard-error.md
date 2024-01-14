---
title:                "Gleam: 표준 오류에 쓰는 것"
simple_title:         "표준 오류에 쓰는 것"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 오류에 쓰기를 하는 이유는 무엇일까요? 

보통 프로그래밍을 할 때에는 결과를 표준 출력으로 출력하곤 합니다. 하지만 때로는 오류가 발생했을 때 이를 표시하기 위해 표준 오류에 쓰기를 사용해야 할 수도 있습니다. 예를 들어, 디버깅을 할 때나 예외 처리를 할 때에는 이 기능이 유용하게 쓰일 수 있습니다.

## 어떻게 하나요

Gleam에서 표준 오류에 쓰기를 하는 방법은 매우 간단합니다. 다음 코드를 참고해주세요.

```Gleam
import gleam/io as io 

fn main() {
  io.stderr.print("이 문장은 표준 오류에 출력됩니다.")
}
```

위 코드는 "이 문장은 표준 오류에 출력됩니다."라는 메시지를 표준 오류에 출력하는 간단한 예시입니다. 실행하면 다음과 같은 결과가 나타납니다.

```
이 문장은 표준 오류에 출력됩니다.
```

위 코드에서 가장 중요한 부분은 `io.stderr.print`라는 함수입니다. 이 함수는 표준 오류에 바로 출력할 수 있도록 해줍니다.

## 깊이 파기

표준 오류에 쓰기를 하는 것은 간단해보이지만 실제로는 더 많은 일이 일어나고 있습니다. 하지만 많은 경우 이를 모르고 있어도 문제없이 사용할 수 있습니다. 하지만 깊이 파고들어 이에 대해 더 알아보고 싶다면 Gleam 공식 문서를 참고하는 것을 추천드립니다.

## 더 보기

- Gleam 공식 문서: https://gleam.run/documentation/
- 표준 출력에 쓰기: https://kb.iu.edu/d/admm
- 표준 오류에 쓰는 이유: https://stackoverflow.com/questions/600635/what-is-the-difference-between-stderr-and-stdout