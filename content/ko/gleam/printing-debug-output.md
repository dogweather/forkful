---
title:                "Gleam: 디버그 출력 프린팅"
simple_title:         "디버그 출력 프린팅"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜 

디버그 출력을 인쇄하는 이유는 코드에서 발생하는 문제를 해결하는 데 도움이 되기 때문입니다.

## 어떻게 

```Gleam
use gleam/debug

fn main() {
  let num = 5
  let double = num * 2
  debug.print("num: ", num)
  debug.print("double: ", double)
}
```

출력 결과:
```
[debug] num: 5
[debug] double: 10
```

위의 예제에서는 디버그 출력을 사용하여 변수 `num`과 `double`의 값을 확인할 수 있습니다.

## 깊게 파헤치기 

디버그 출력에는 여러가지 옵션들이 있습니다. `debug.print` 함수에는 두 개 이상의 인자를 전달할 수도 있으며, 여러 줄에 걸쳐 출력할 수도 있습니다. 또한, `debug.log` 함수를 사용하여 출력 내용을 파일에 저장할 수도 있습니다.

또한, `debug.assert` 함수를 사용하여 조건문을 확인하고 해당 조건이 참이 아닐 경우 코드가 멈추도록 할 수도 있습니다. 디버그 출력을 통해 어떤 조건에서 코드가 멈췄는지 확인할 수 있습니다.

## See Also 

- [Gleam 공식 문서](https://gleam.run/documentation/getting-started#debug-logging)
- [Gleam 레포지토리의 디버깅 예제](https://github.com/gleam-lang/gleam/blob/master/examples/debugging.gleam)
- [Gleam 커뮤니티 포럼](https://elixirforum.com/c/gleam-lang)