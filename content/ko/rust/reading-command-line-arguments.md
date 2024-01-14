---
title:                "Rust: 컴퓨터 프로그래밍으로 엔터키를 통해 읽는 명령줄 인수 (Command Line Arguments에 대한)"
simple_title:         "컴퓨터 프로그래밍으로 엔터키를 통해 읽는 명령줄 인수 (Command Line Arguments에 대한)"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
커맨드 라인 인자를 읽는 것은 프로그램을 만드는 과정에서 중요한 부분입니다. 커맨드 라인 인자를 활용하면 프로그램의 실행 시 인자들을 설정하거나 추가적인 기능을 제공할 수 있습니다.

## How To
커맨드 라인 인자를 읽는 방법은 Rust에서는 매우 간단합니다. 우선 std::env 모듈을 import 해야합니다. 그리고 다음과 같이 `args` 함수를 호출하면 모든 커맨드 라인 인자를 Vec<String> 타입으로 받아올 수 있습니다.

```
Rust
use std::env;

fn main() {
  let args: Vec<String> = env::args().collect();

  println!("Command line arguments: {:?}", args);
}
```

위와 같이 프로그램에서 받은 인자들을 `args` 변수에 저장하고, 원하는 대로 활용하면 됩니다. 실행해보면 다음과 같은 결과가 나옵니다.

```
> myprogram arg1 arg2
Command line arguments: ["myprogram", "arg1", "arg2"]
```

커맨드 라인 인자가 없을 경우, `args` 변수에는 프로그램의 이름만 저장되니 유의해야 합니다. 또한 인자의 개수에 따라서 다양한 처리를 할 수도 있습니다. 예를 들어, 프로그램 이름과 인자 2개를 받는 상황에서 다음과 같이 처리할 수 있습니다.

```
Rust
fn main() {
  let args: Vec<String> = env::args().collect();
  let program_name = &args[0];
  let arg1 = &args[1];
  let arg2 = &args[2];

  println!("Program: {}", program_name);
  println!("Argument 1: {}", arg1);
  println!("Argument 2: {}", arg2);
}
```

실행해보면 다음과 같은 결과가 나옵니다.

```
> myprogram arg1 arg2
Program: myprogram
Argument 1: arg1
Argument 2: arg2
```

## Deep Dive
커맨드 라인 인자를 좀 더 깊이 이해하기 위해서는 Rust에서 커맨드 라인 인자를 어떻게 처리하는지 살펴보는 것도 도움이 될 수 있습니다. Rust에서는 `Opts` trait를 이용하여 커맨드 라인 인자를 처리하는 방식을 지정할 수 있습니다. 또한, `clap` crate를 이용하면 보다 쉽게 커맨드 라인 인자를 처리할 수 있습니다.

## See Also
- [Rust Documentation - std::env 모듈](https://doc.rust-lang.org/std/env/index.html)
- [Rust Documentation - clap crate](https://docs.rs/clap/2.33.3/clap/)