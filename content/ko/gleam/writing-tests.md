---
title:                "Gleam: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

프로그래머는 코드를 테스트하는 것이 중요합니다. 이를 통해 코드의 예기치 않은 버그를 발견하고 수정할 수 있으며, 코드의 안정성과 신뢰성을 높일 수 있습니다. Gleam을 사용하여 테스트를 작성하면 이러한 이점을 쉽게 얻을 수 있습니다.

## 어떻게

Gleam은 함수형 프로그래밍 언어로, 테스트 작성에 있어서도 독특한 방법을 제공합니다. 먼저, `gleam/standard_library` 패키지에서 `test` 모듈을 임포트합니다. 그리고 아래와 같이 테스트 함수를 정의합니다.

```Gleam
pub fn example_test() {
    assert.equal(2, 1 + 1)
}
```

위 예제는 `equal` 함수를 사용하여 예상 값과 실제 값이 같은지 확인하는 테스트를 작성하는 방법을 보여줍니다. 이 함수는 위와 같이 `expected`와 `actual` 두 개의 매개변수를 받아서 비교합니다. 만약 두 값이 같지 않으면 테스트는 실패합니다.

만약 예외 상황을 테스트하고 싶다면 `assert.error` 함수를 사용할 수 있습니다. 예를 들어, 아래와 같은 코드를 통해 어떻게 예외를 처리하는지 확인할 수 있습니다.

```Gleam
pub fn division_test() {
    assert.error("Division by zero", fn() {
        1 / 0
    })
}
```

위 예제에서는 `1/0`의 결과가 예외를 발생시키는 것을 기대합니다. 따라서 `assert.error` 함수의 첫 번째 매개변수로 예상한 예외 메시지를 전달하고, 두 번째 매개변수로 예외가 발생할 코드를 작성합니다.

더 많은 코드 예제와 테스트 출력 결과 등은 [Gleam 공식 문서](https://gleam.run/book/tour/testing.html)에서 확인할 수 있습니다.

## 딥 다이브

Gleam에서는 테스트를 작성할 때 `setup`과 `teardown` 함수를 사용할 수 있습니다. `setup` 함수는 각 테스트 함수 실행 전에 실행되며, `teardown` 함수는 각 테스트 함수 실행 후에 실행됩니다. 이 함수들을 사용하면 각 테스트 함수마다 반복적으로 필요한 작업을 할 필요 없이, 한 번만 작업을 정의하면 됩니다.

또한 Gleam에서는 모든 테스트 함수를 한 번에 실행하는 `run_all` 함수도 제공합니다. 이를 사용하면 여러 테스트 함수를 한 번에 실행하여 일일이 실행되는 번거로움을 없앨 수 있습니다.

이외에도 Gleam은 다양한 테스트 기능을 지원하고 있으며, 이를 사용하면 더욱 효율적이고 체계적으로 테스트를 작성할 수 있습니다. 자세한 내용은 [Gleam 공식 문서](https://gleam.run/book/tour/testing.html)에서 확인할 수 있습니다.

## 참고

- [Gleam 공식 문서](https://gleam.run/book/tour/testing.html)
- [Gleam 테스트 예제 저장소](https://github.com/gleam-lang/testing_examples)
- [Gleam Slack 커뮤니티](https://gleam-slackin.herokuapp.com/)