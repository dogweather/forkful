---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 뭐하고 왜?

텍스트 파일을 읽는 것은 텍스트 파일의 데이터를 프로그램 내로 가져오는 과정입니다. 이 작업을 통해 개발자들은 저장된 정보를 분석하거나 재활용하는 등의 다양한 작업을 수행합니다.

## 어떻게 하나:

Rust에서 텍스트 파일 읽기:

```Rust
use std::fs::File;
use std::io::Read;

fn main() -> std::io::Result<()> {
    let mut file = File::open("example.txt")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    println!("{}", content);
    Ok(())
}
```

이 방법으로 출력은 이렇게 나올 것입니다. 

```Rust
Hello, world!
Hi, Rust!
```
## 깊은 이야기:

텍스트 파일 읽기는 자동화, 데이터 분석 등의 많은 작업을 위해 필요한 기본적인 프로그래밍 작업입니다. 읽기 작업의 역사는 컴퓨터의 초기부터 시작되었습니다. 

Rust에서는 `Read` trait를 이용하면 다양한 데이터 소스로부터 데이터를 읽어올 수 있습니다. 이와 달리 Python에서는 `open` 함수를 이용하여 파일을 열고, `read` 메소드로 데이터를 읽어옵니다.

Rust에서 텍스트 파일을 읽는 또 다른 방법으로 `BufRead` trait를 이용할 수 있습니다. 이 방법은 한 번에 한 라인씩 데이터를 읽어 올 수 있으므로 큰 용량의 파일을 처리할 때 유용합니다. 

## 참고 자료:

1. [Rust Book - File I/O](https://doc.rust-lang.org/stable/book/ch12-02-reading-a-file.html)
3. [Rust API - std::fs::File](https://doc.rust-lang.org/std/fs/struct.File.html)