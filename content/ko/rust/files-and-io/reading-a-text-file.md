---
date: 2024-01-20 17:55:40.838324-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) Rust\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\
  \uC77C\uC744 \uC77D\uAE30 \uC704\uD55C \uAE30\uBCF8\uC801\uC778 \uBC29\uBC95\uC740\
  \ `std::fs` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4\
  . \uC544\uB798 \uC608\uC2DC \uCF54\uB4DC\uC5D0\uC11C\uB294 \uAC04\uACB0\uD558\uAC8C\
  \ \uD30C\uC77C \uC77D\uAE30\uB97C \uBCF4\uC5EC\uC90D\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.941455-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:)\nRust\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744\
  \ \uC77D\uAE30 \uC704\uD55C \uAE30\uBCF8\uC801\uC778 \uBC29\uBC95\uC740 `std::fs`\
  \ \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## How to:
(어떻게:)
Rust에서 텍스트 파일을 읽기 위한 기본적인 방법은 `std::fs` 모듈을 사용하는 것입니다. 아래 예시 코드에서는 간결하게 파일 읽기를 보여줍니다.

```Rust
use std::fs;

fn main() -> std::io::Result<()> {
    let content = fs::read_to_string("example.txt")?;
    println!("File content:\n{}", content);
    Ok(())
}
```

위 코드를 실행하면 `example.txt` 파일의 내용을 불러와 출력합니다.

## Deep Dive:
(심층 탐구)
Rust의 파일 입출력 기능은 `std::fs`와 `std::io` 모듈에 핵심적으로 내장되어 있습니다. Rust 1.0이 출시된 후, 파일 입출력 API는 안전하고 직관적인 사용을 중심으로 진화해왔습니다.

예외 처리 대신 `Result` 타입을 사용함으로써, 파일을 읽을 때 발생할 수 있는 에러를 명시적으로 다룹니다. 이런 접근 방식은 명확한 에러 핸들링을 가능하게 해줍니다.

파일을 읽는 대안으로는 `std::io::BufReader`를 사용할 수 있습니다. 큰 파일을 처리할 때, 이 방법은 메모리 사용량을 절감하고 성능을 향상시킬 수 있습니다.

```Rust
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let path = Path::new("example.txt");
    let file = File::open(&path)?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        println!("{}", line?);
    }
    Ok(())
}
```

`BufReader`는 파일을 줄 단위로 효율적으로 읽어 처리할 수 있게 해줍니다.

## See Also:
(참고 자료)
- [The Rust Programming Language - std::fs](https://doc.rust-lang.org/std/fs/)
- [The Rust Programming Language - std::io](https://doc.rust-lang.org/std/io/)
- [Rust by Example - File I/O (Read)](https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html)
- [Rust Book - Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
