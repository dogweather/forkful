---
title:                "에러 처리하기"
date:                  2024-01-26T00:52:52.207381-07:00
model:                 gpt-4-1106-preview
simple_title:         "에러 처리하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
오류 처리는 코드에서 잘못될 수 있는 점들을 예상하고 그러한 상황들을 우아하게 관리하는 것입니다. 이러한 작업을 프로그래머들이 하는 이유는 애플리케이션을 견고하고 사용자 친화적으로 유지하기 위해서이며, 예상치 못한 문제에 직면했을 때에도 마찬가지입니다.

## 방법:
Gleam에서는 종종 오류 처리를 위해 `Result` 타입을 사용합니다. 이것은 `Ok` (성공을 위한)와 `Error` (실패를 위한)의 두 가지 변형을 가진 열거형입니다. 다음은 간단한 예입니다:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("오잉! 고장 났습니다.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

`main`을 `might_fail(False)`로 실행하면 `42`를 반환합니다. `True`를 전달하면 "오잉! 고장 났습니다."를 출력하고 `0`을 반환합니다.

## 깊은 이해
Gleam의 오류 처리 방식은 그것의 Erlang 뿌리에 영향을 받았습니다. 역사적으로 Erlang은 "그냥 충돌 나도록 두기"라는 철학을 사용하여, 감시 트리(supervision trees)를 통해 프로세스 실패를 관리합니다. 그러나, 감시될 의도가 없는 프로세스 내에서 Gleam 코드를 작성할 때, 예를 들어 라이브러리 함수 내에서는 오류를 명시적으로 처리하고 싶을 것입니다.

`Result`를 사용하는 것 외의 대안으로는 무언가가 `None` (아무것도 없음) 또는 `Some` (어떤 것)일 수 있는 경우를 위해 `Option` 타입을 사용할 수 있지만, 이들은 오류 정보를 담고 있지 않습니다. 프로세스 경계를 넘어서 오류를 신호하는 경우에는 Erlang의 메시지 전달 메커니즘을 사용할 수 있습니다.

Gleam의 오류 처리는 함수형 프로그래밍 스타일을 반영하며, 여기서 부작용(예를 들어 오류들)은 타입과 패턴 매칭을 사용해서 관리되어 오류 관리에 있어서 명확성과 예측 가능성을 제공합니다.

## 참고 자료
- [Erlang의 오류 처리](http://erlang.org/doc/reference_manual/errors.html)
