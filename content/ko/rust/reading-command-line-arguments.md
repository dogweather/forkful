---
title:                "명령줄 인수 읽기"
html_title:           "Rust: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 무엇이며 왜 해야하나요?

커맨드 라인 인자를 읽는 것은 프로그래머들이 커맨드 라인에서 사용자로부터 입력을 받기 위해서 하는 것입니다. 보통 프로그램이 실행될 때, 사용자는 프로그램에게 추가 정보를 제공하기 위해 커맨드 라인 인자를 전달합니다. 따라서 이를 읽는 것은 더 유연한 프로그램을 만드는 데에 도움이 됩니다.

## 방법:

Rust에서는 `std::env` 모듈을 사용하여 커맨드 라인 인자를 읽을 수 있습니다. `std::env::args()` 함수를 호출하여 커맨드 라인 인자를 벡터로 받아올 수 있습니다. 이 벡터를 순환하며 인자를 사용할 수 있습니다. 아래는 커맨드 라인에서 `name` 인자를 받아 출력하는 간단한 예제입니다.

```
Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    let name = &args[1];

    println!("Hello, {}!", name);
}
```

커맨드 라인에서 `cargo run` 명령어를 사용하여 위 코드를 실행하면 `Hello, [name]!` 형식으로 인자에 입력한 값이 출력됩니다.

## 깊게 들어가기:

커맨드 라인 인자를 처음 사용한 것은 1960년대에 메인 프레임에서 프로그램을 실행하기 위해 사용되었습니다. 현재 대부분의 OS들은 `argc`와 `argv` 매개변수를 제공하여 프로그램에 대한 커맨드 라인 인자를 전달할 수 있도록 지원하고 있습니다.

Rust에서는 `std::env` 외에도 `clap`이라는 오픈 소스 라이브러리를 사용하여 더 유연하고 강력한 방식으로 커맨드 라인 인자를 읽을 수 있습니다. `clap`은 Rust 커뮤니티에서 많이 사용되고 있는 라이브러리이며 다양한 기능을 지원합니다.

커맨드 라인 인자를 읽는 방식은 환경에 따라 다를 수 있지만, 보통은 문자열로 전달됩니다. 따라서 문자열 처리에 대한 이해가 필요합니다. 또한 Rust에서는 우리가 전달받는 인자에 대한 타입 정보를 정적으로 추론하여 사용할 수도 있습니다.

## 관련 자료:

- [Rust Book - CLI Arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- [The `env` module](https://doc.rust-lang.org/std/env/index.html)
- [The `clap` library](https://crates.io/crates/clap)