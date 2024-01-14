---
title:                "Gleam: 테스트 작성하기"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# 왜 테스트를 작성해야 할까요?
우리는 모든 소프트웨어를 개발하는 동안 항상 버그가 발생할 수 있음을 인지해야 합니다. 따라서 테스트를 작성하는 것은 우리가 개발한 코드가 정확하게 작동하는지 확인하는 중요한 부분입니다. 또한 테스트를 통해 우리는 코드를 쉽게 확장하고 수정할 수 있도록 안정성을 보장할 수 있습니다.

## 어떻게 하나요?
```Gleam
test("덧셈 함수를 테스트합니다.", fn() {
  expect(add(2, 3)).toBe(5)
})

fn add(x, y) {
  x + y
}
```

위의 코드를 보면, 우리는 `add` 함수를 정의하고 해당 함수를 테스트하고 있습니다. `test` 함수에는 테스트의 이름과 실행할 코드를 전달합니다. `expect` 함수는 우리가 예상하는 결과를 지정하고 `toBe` 함수를 통해 실제 결과와 비교합니다.

## 깊은 이해
테스트는 우리가 개발한 소프트웨어의 안정성을 보장하는 중요한 부분입니다. 따라서, 우리는 코드를 작성할 때 테스트 가능하도록 구성하는 것이 중요합니다. 그리고 모든 가능한 케이스를 고려해서 테스트를 작성하는 것이 좋습니다.

# 또 다른 정보들
- [Gleam 공식 웹사이트](https://gleam.run/)
- [Gleam 테스트 관련 문서](https://gleam.run/documentation/testing)
- [Gleam 테스트 코드 예제](https://github.com/gleam-lang/gleam/blob/main/lib/gleam/test/examples.test)