---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

명령줄 인자 읽기는 프로그램이 외부에서 데이터를 입력 받는 방식 중 하나입니다. 프로그래머들은 이를 이용해 사용자로부터 여러 형태의 동적 데이터를 입력 받아 프로그램의 동작을 조절합니다.

## 어떻게 사용하는가?

Rust에서 명령줄 인자를 읽기 위한 가장 기본적인 방법은 `std::env::args` 함수를 이용하는 것입니다.

```Rust
fn main() {
    let args: Vec<String> = std::env::args().collect();

    for arg in args {
        println!("'{}'", arg);
    }
}
```

위 코드를 실행하면 프로그램에 전달된 모든 인자들이 출력됩니다. 

예를 들어, `cargo run --example args foo bar baz`를 실행하면 다음과 같은 결과가 나옵니다.

```bash
"target/debug/examples/args"
"foo"
"bar"
"baz"
```
 
## 깊게 알아보기

### 역사적 배경

명령줄 인자는 UNIX와 같은 고전적인 운영체제부터 사용되어 왔습니다. 이 기능은 프로그램을 동적으로 구성하고, 텍스트 기반의 인터페이스에서 사용자의 입력에 반응할 수 있게 해 줌으로써 매우 유용하다는 것이 입증되었습니다.

### 대안

Rust에서는 `std::env::args` 외에도 명령줄 인자 작업을 돕는 여러 패키지가 존재합니다. 그 중 `getopts`나 `clap`와 같은 패키지는 복잡한 인자 분석과 검증, 에러 메시지 생성 등 더 많은 기능을 제공합니다.

### 구현 세부 사항

`std::env::args`는 프로그램 시작 시 인자를 String 형태의 Vector로 변환하여 반환합니다. 이는 Rust의 소유권 모델과 잘 맞아떨어지며, 안전한 파싱을 가능하게 합니다.

## 참고 링크

- [Rust Documentation: std::env](https://doc.rust-lang.org/std/env/index.html)
- [The Rust Programming Language: Command line arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)