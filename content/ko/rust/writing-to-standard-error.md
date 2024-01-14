---
title:                "Rust: 표준 에러에 쓰기"
simple_title:         "표준 에러에 쓰기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
프로그래밍을 하다 보면 에러 메시지를 볼 수 있습니다. 에러 메시지를 이해하고 해결하는 데에 도움을 주기 위해 표준 에러에 대한 쓰기 방법을 알아보겠습니다.

## 방법
먼저, Rust에서 표준 에러를 쓰기 위해서는 `std::io::stderr` 모듈을 사용해야 합니다. 이 모듈은 에러를 쓰는 데에 필요한 여러 가지 함수를 제공합니다. 예를 들어, 다음 예제 코드는 표준 에러에 메시지를 쓰고 있습니다.

```Rust
use std::io::{self, Write};

fn main() {
    let error_message = "에러 발생!";
    let result = writeln!(&mut io::stderr(), "{}", error_message); // 표준 에러에 메시지 쓰기
    if result.is_err() {
        println!("표준 에러 쓰기에 실패했습니다.");
    }
}
```

위의 코드를 실행하면, 표준 에러에 "에러 발생!"이라는 메시지가 쓰여지는 것을 확인할 수 있습니다. 이렇게 표준 에러에 메시지를 쓰면, 사용자에게 더 자세한 정보를 제공할 수 있고, 프로그램의 디버깅에도 도움이 됩니다.

## 깊이 파고들기
표준 에러를 사용하는 더 복잡한 예제를 살펴보겠습니다. 아래 코드는 파일을 읽어서 그 내용을 표준 에러에 출력하는 예제입니다.

```Rust
use std::fs::File;
use std::io::{self, BufReader};
use std::io::prelude::*;

fn main() {
    let file = File::open("test.txt").unwrap();
    let reader = BufReader::new(file);
    let mut stderr = io::stderr();

    for line in reader.lines() {
        match line {
            Ok(content) => {
                writeln!(&mut stderr, "{}", content).unwrap(); // 표준 에러에 파일 내용 쓰기
            },
            Err(error) => {
                writeln!(&mut stderr, "{}", error).unwrap(); // 에러가 발생하면 표준 에러에 에러 메시지 쓰기
            }
        }
    }
}
```

위의 코드를 실행하면, `test.txt` 파일의 내용이 표준 에러에 출력되는 것을 확인할 수 있습니다. 이 예제에서는 `writln!` 매크로를 사용해 문자열을 표준 에러에 쓰지만, 각 직접적으로 `write` 메서드를 사용해도 동일한 결과를 얻을 수 있습니다.

## 참고
- Rust 공식 문서 - [std::io::stderr](https://doc.rust-lang.org/std/io/fn.stderr.html)
- Rust by Example - [Formatted Printing](https://doc.rust-lang.org/stable/rust-by-example/hello/print/print_debug.html)
- The Rust Programming Language - [Handling Errors](https://doc.rust-lang.org/book/second-edition/ch09-00-error-handling.html)

## 참고하기
- [std::io::stderr 공식 문서](https://doc.rust-lang.org/std/io/fn.stderr.html)
- [표준 에러와 에러 핸들링에 관한 Rust by Example 글](https://doc.rust-lang.org/stable/rust-by-example/hello/print/print_debug.html)
- [에러 핸들링에 관한 The Rust Programming Language 챕터](https://doc.rust-lang.org/book/second-edition/ch09-00-error-handling.html)