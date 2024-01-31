---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
텍스트 파일 작성이란 문자열 데이터를 파일로 저장하는 것입니다. 프로그래머들은 데이터 로깅, 설정 관리, 사용자 데이터 저장 등을 위해 사용합니다.

## How to:
```Rust
use std::fs::File;
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let mut file = File::create("output.txt")?;
    file.write_all("안녕하세요, Rust 프로그래밍!".as_bytes())?;
    Ok(())
}
```
```plaintext
// output.txt 파일에는 다음의 내용이 저장됩니다.
안녕하세요, Rust 프로그래밍!
```

## Deep Dive
텍스트 파일 작성은 UNIX 시스템의 초기부터 시작되었습니다. Rust에서는 `std::fs`와 `std::io` 모듈을 사용해 간결하고 안전한 API를 제공합니다. `File::create`는 파일을 새로 만들고, `write_all` 메서드는 데이터의 무결성을 보장하며 씁니다. 쓰기 도중에 발생 가능한 오류를 대처하기 위해 `io::Result` 타입이 사용됩니다. 대안으로 `std::fs::write` 함수를 한 줄로 간단하게 쓸 수도 있지만, 더 세밀한 제어가 필요할 때는 `File`을 사용하는 것이 더 좋습니다.

## See Also
- [Rust 문서: std::fs](https://doc.rust-lang.org/std/fs/index.html)
- [Rust 문서: std::io](https://doc.rust-lang.org/std/io/index.html)
- [Rust by Example: 파일 읽고 쓰기](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
