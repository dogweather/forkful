---
title:    "Rust: 컴퓨터 프로그래밍: 명령 줄 인수 읽기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜

Rust 프로그래밍을 할 때, 사람들은 커맨드 라인 인자(argument)를 읽어내는 게 필요할 때가 있다. 그 이유는 애플리케이션의 설정을 바꾸거나, 다른 기능을 활성화할 때이다.

## 어떻게

커맨드 라인 인자를 읽어오기 위해서는 `std::env` 모듈의 `args()` 함수를 사용할 수 있습니다. 이 함수는 `std::env::Args` 타입을 반환하며, 이 인터페이스를 통해 여러 가지 메소드를 사용할 수 있습니다.

예제 코드:

```Rust
use std::env;

fn main() {
    // 현재 실행중인 프로그램의 인자들을 가져온다.
    let args: Vec<String> = env::args().collect();

    // 첫 번째 인자는 항상 프로그램의 경로이기 때문에 무시한다.
    let arguments = &args[1..];

    // `for` 루프를 사용하여 각 인자를 출력한다.
    for arg in arguments {
        println!("{}", arg);
    }
}
```

실행:
```
$ rustc main.rs
$ ./main hello world
hello
world
```

## 딥 다이브

커맨드 라인 인자는 `main()` 함수의 매개변수로도 사용할 수 있습니다. 이 경우, `env::args()` 함수를 사용하지 않고도 인자들을 직접 접근할 수 있습니다.

예제 코드:

```Rust
fn main() {
    // `args` 매개변수를 사용하여 인자들을 접근할 수 있습니다.
    for arg in std::env::args() {
        println!("{}", arg);
    }
}
```

실행:
```
$ rustc main.rs
$ ./main hello world
./main
hello
world
```

# 참고

- [Rust 공식 문서: Command Line Arguments](https://doc.rust-lang.org/std/env/fn.args.html)
- [Rust By Example: Command Line Arguments](https://rustbyexample.com/std_misc/arg.html)