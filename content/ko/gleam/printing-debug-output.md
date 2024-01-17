---
title:                "디버그 출력 프린팅"
html_title:           "Gleam: 디버그 출력 프린팅"
simple_title:         "디버그 출력 프린팅"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디버그 출력을 하는 것은 우리가 작성한 코드의 실행 과정을 파악하고 문제를 해결하는 데에 도움이 됩니다. 프로그래머들은 코드를 효과적으로 디버깅하기 위해 디버그 출력을 종종 사용합니다.

## 방법:

디버그 출력은 Gleam에서 간단하게 할 수 있습니다. `gleam_debug` 모듈을 가져오고, `p` 함수를 사용하여 변수나 값을 쉽게 출력할 수 있습니다. 아래 코드는 Gleam에서 디버그 출력을 하는 예시입니다.

```Gleam
import gleam_debug

let languages = ["Gleam", "Rust", "Elixir"]
p(languages) // ["Gleam", "Rust", "Elixir"]
```

위 코드의 결과는 `["Gleam", "Rust", "Elixir"]`로 출력됩니다. 이처럼 `p` 함수를 사용하면 변수나 값을 쉽게 확인할 수 있습니다.

## 깊이 들어가보기:

디버그 출력은 많은 프로그래밍 언어에서 사용되고 있으며, Gleam에서도 자주 활용됩니다. Gleam의 경우, `gleam_debug` 모듈을 사용하여 디버그 출력을 할 수 있습니다. 또 다른 대안으로는 `gleam_assert` 모듈을 사용하여 조건을 확인하고 디버그 출력을 할 수도 있습니다. 디버그 출력은 코드의 실행 과정을 추적하고 문제를 해결하는 데에 큰 도움이 됩니다.

## 관련 자료:

- [Gleam 공식 문서](https://gleam.run/)
- [Rust vs. Elixir: Which One to Pick?](https://www.nexsoftsys.com/articles/rust-vs-elixir-which-one-to-pick.html)
- [디버깅에 대한 과정 이해하기](https://www.techempower.com/blog/2019/03/07/debugging-an-exercise-in-confirmation-bias/)