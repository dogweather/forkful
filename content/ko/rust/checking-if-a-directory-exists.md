---
title:                "Rust: 디렉토리가 있는지 확인하기"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜: 디렉토리가 있는지 확인하는 것이 왜 중요한가?
디렉토리를 확인하는 것은 프로그래밍에서 중요한 과정입니다. 예를 들어, 파일 시스템 내에 존재하지 않는 디렉토리를 확인하고자 할 때, 프로그램이 예외를 던지거나 다른 방법으로 처리할 필요가 있을 수 있습니다.

## 어떻게: Rust에서 디렉토리가 있는지 확인하는 방법
```Rust
use std::path::Path;
fn main() {
    let path = Path::new("directory_name");
    if path.exists() {
        println!("The directory exists.");
    } else {
        println!("The directory does not exist.");
    }
}
```
위 예제는 `std::path::Path` 모듈을 사용하여 디렉토리가 존재하는 지 확인하는 간단한 코드입니다. `exists()` 메서드를 사용하여 디렉토리가 존재하는 경우 `true`를 반환하고, 그렇지 않은 경우 `false`를 반환합니다. 이를 활용하여 적절한 예외 처리나 다양한 로직을 구현할 수 있습니다.

## 딥 다이브: 디렉토리 존재 확인에 대해 더 알아보기
디렉토리가 있는지 확인하는 방법이 매우 간단하다고 생각할 수 있지만 실제로는 여러 가지 경우에 따라 다르게 구현될 수 있습니다. 예를 들어, 디렉토리가 존재하지 않는 경우 예외를 던지는 것이 아닌 다른 방식으로 처리하는 경우에는 `Path::new()` 대신 `Path::new_dir()` 를 사용하여 디렉토리를 생성할 수 있습니다. 또한, 다른 모듈에서 `exists()` 메서드를 활용하는 등 더 다양한 활용 방법이 존재합니다.

## 더 알아보기
[The Rust Programming Language - Path](https://doc.rust-lang.org/std/path/index.html)

[Rust by Example - Paths](https://doc.rust-lang.org/stable/rust-by-example/std_misc/file/create.html)

[Rust Documentation - Checking for the existence of a path](https://doc.rust-lang.org/std/fs/struct.File.html#method.create)