---
title:    "Gleam: 디버그 출력하기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

Gleam 프로그래밍 블로그 글 - 디버그 출력을 하는 이유

## Why
디버그 출력을 하면 개발 과정에서 발생하는 코드 상의 오류를 식별하고 수정하는 데 도움이 됩니다.

## How To
```Gleam
fn main() {
  let x = 5
  let y = 7
  let sum = add(x, y)
  debug!(sum, "The sum of {x} and {y} is {sum}")
}
```

위의 코드에서는 두 수를 더한 결과를 `debug!` 매크로를 사용하여 출력하고 있습니다. 이렇게 출력되는 정보는 우리가 원하는 형식으로 지정할 수 있으며, 변수의 값을 알아보기 쉬우므로 디버그에 매우 유용합니다.

## Deep Dive
디버그 출력은 코드에서 발생하는 다양한 오류를 식별하고 디버깅하는 데 꼭 필요한 기능입니다. 여러분은 `debug!` 매크로를 사용하여 출력 내용을 원하는 대로 형식화할 수 있으며, 이를 통해 오류를 더 쉽게 찾을 수 있습니다. 또한, `debug!` 매크로를 활용하면 필요한 정보만 출력하도록 필터링할 수도 있습니다.

See Also
- [Gleam 공식 문서](https://gleam.run/)
- [Gleam 커뮤니티 포럼](https://forum.gleam.run/)
- [Gleam 공식 GitHub 레포지토리](https://github.com/gleam-lang/gleam)