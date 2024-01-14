---
title:    "Gleam: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 왜
커맨드 라인 인자를 읽는 것이 중요한 이유는, 프로그래머가 사용자가 입력한 정보를 프로그램에 적용할 수 있기 때문입니다.

# 방법
아래 코드 블록에서는 Gleam 언어를 사용하여 커맨드 라인 인자를 읽는 방법을 보여줍니다. 각 코드 블록의 아래에는 예상되는 출력이 나와 있습니다.

```Gleam
import gleam/io

fn main() {
    // 커맨드 라인에서 두 개의 인자를 읽어옵니다
    let argument1 = io.args()[0]
    let argument2 = io.args()[1]

    // 읽어온 인자를 화면에 출력합니다
    io.println("첫 번째 인자: {}", [argument1])
    io.println("두 번째 인자: {}", [argument2])
}
```

예상 출력:
```
> gleam run read_args.gleam argument1 argument2
첫 번째 인자: argument1
두 번째 인자: argument2
```

# 깊이 들어가기
커맨드 라인 인자를 읽는 것은 프로그래밍의 한 가지 기본적인 기술입니다. 커맨드 라인 인자는 프로그램을 실행하는 데 필요한 정보를 제공하는 데 사용될 수 있습니다.그럼에도 불구하고, 커맨드 라인 인자를 정확하게 읽는 것은 때로 복잡할 수 있습니다. 만약 인자가 공백을 포함하거나 정수형으로 지정되어있다면 어떻게 해야 할까요? 프로그래머는 이러한 상황을 고려하여 적절한 처리 방법을 구현해야 합니다.

# 참고
[Gleam 문서: 커맨드 라인 인자](https://gleam.run/documentation/cmdline_arguments)
[일반적인 커맨드 라인 인자 처리 패턴](https://wiki.python.org/moin/HandlingExceptions)