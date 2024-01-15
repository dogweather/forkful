---
title:                "임시 파일 만들기"
html_title:           "Rust: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

어느 시점에 하나의 임시 파일을 만들어야 하는 이유가 있을 수 있습니다. 환경 설정, 중간 결과물 저장 등 다양한 경우가 있지만, 이 글에서는 특히 Rust 언어에서 임시 파일을 만드는 방법을 다루고 있습니다.

## 어떻게

이번 섹션에서는 Rust 언어를 사용하여 임시 파일을 만드는 방법을 실제 코드 예제와 함께 설명하겠습니다.

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // 임시 파일을 만들기 위해 필요한 파일명을 지정합니다.
    let file_name = "temp.txt";

    // `File::create()` 메서드를 사용하여 파일을 생성합니다.
    // 결과적으로 `File` 구조체의 인스턴스가 반환됩니다.
    let mut file = File::create(file_name).unwrap();

    // 파일에 텍스트를 작성합니다.
    file.write_all(b"Hello, world!").unwrap();
}
```

위의 코드는 `temp.txt` 파일을 생성하고, 그 안에 "Hello, world!"라는 텍스트를 작성하는 예제입니다. 이렇게 작성된 임시 파일은 현재 디렉토리에 생성되며, 프로그램이 종료될 때 자동으로 삭제됩니다.

```Rust
// 이전과 같은 내용
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let file_name = "temp.txt";
    let mut file = File::create(file_name).unwrap();
    file.write_all(b"Hello, world!").unwrap();
    
    // 임시 파일을 생성한 후, 다시 열어서 읽어올 수도 있습니다.
    let mut temp_file = File::open(file_name).unwrap();
    let mut contents = String::new();
    temp_file.read_to_string(&mut contents).unwrap();
    println!("{}", contents); // "Hello, world!"
}
```

위의 코드는 임시 파일을 생성한 후, 다시 해당 파일을 열어서 내용을 읽어오는 예제입니다. 이렇게 임시 파일을 다시 열어 사용하는 것도 가능합니다.

## 더 깊이 들어가보기

Rust 언어에서 임시 파일을 만들 때, `File::create()` 메서드를 사용하는 것 이외에도 여러 가지 방법이 있습니다. `tempfile` 라이브러리를 사용하면, 더욱 간단하고 유연하게 임시 파일을 생성할 수 있습니다. 또한, 임시 파일의 이름을 랜덤하게 생성하고 파일 디스크립터를 반환하는 `mkstemp` 함수를 사용할 수도 있습니다.

## 관련 자료

- [Rust 표준 라이브러리 문서 - `std::fs::File::create()`](https://doc.rust-lang.org/std/fs/struct.File.html#method.create)
- [Rust 표준 라이브러리 문서 - `std::io::prelude` 모듈](https://doc.rust-lang.org/std/io/prelude/index.html)
- [Rust by Example - File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file/open.html)
- [tempfile 라이브러리 문서](https://docs.rs/tempfile/)