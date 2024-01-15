---
title:                "컴퓨터 프로그래밍에서의 커맨드 라인 인수 읽기"
html_title:           "Rust: 컴퓨터 프로그래밍에서의 커맨드 라인 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서의 커맨드 라인 인수 읽기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 방법에 대해 배우는 것은 프로그래밍에서 중요한 기술입니다. 이를 통해 유지보수가 용이하고 유연한 프로그램을 만들 수 있으며, 사용자와의 상호작용을 쉽게 구현할 수 있습니다.

## 어떻게

커맨드 라인 인수는 프로그램을 실행할 때 전달되는 정보를 담고 있습니다. Rust에서는 `std::env` 모듈을 사용하여 이 정보에 접근할 수 있습니다. 아래는 간단한 예제 코드입니다.

```rust
use std::env;

fn main() {
    // 커맨드 라인 인수를 Vec<String> 형식으로 가져옵니다.
    let args: Vec<String> = env::args().collect();

    // 프로그램 이름은 항상 첫 번째 인수로 전달됩니다.
    let program_name = &args[0];

    // 다른 인수들은 1부터 시작되는 인덱스로 접근합니다.
    let first_arg = &args[1];

    println!("프로그램 이름: {}", program_name);
    println!("첫 번째 인수: {}", first_arg);
}
```

위 코드를 컴파일하고 다음과 같이 실행하면,

```bash
$ ./program arg1 arg2
```

다음과 같은 결과가 나옵니다.

```
프로그램 이름: ./program
첫 번째 인수: arg1
```

## 더 깊이 들어가기

커맨드 라인 인수는 문자열(`String`)의 벡터(`Vec`) 형태로 전달됩니다. 따라서 여러 개의 인수를 전달받을 수 있으며, 이를 활용하여 프로그램의 동작을 다양하게 설정할 수 있습니다. 또한, Rust에서는 패턴 매칭을 통해 더 효율적으로 인수에 접근할 수 있습니다.

## 참고 자료

- [Rust 공식 문서 - 커맨드 라인 인수 읽기](https://doc.rust-lang.org/std/env/index.html)
- [The Rust Programming Language - Reading Program Arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- [Rust by Example - Command Line Arguments](https://doc.rust-lang.org/stable/rust-by-example/std_misc/arg.html)

## 참고로

커맨드 라인 인수는 주로 터미널 환경에서 프로그램을 실행할 때 사용됩니다. 따라서 터미널을 사용해 보면서 위 코드를 직접 실행하고 다양한 인수를 전달해보는 것을 추천합니다. 이를 통해 더욱 익숙해진다면 프로그래밍의 다양한 분야에서 유용하게 활용할 수 있을 것입니다.