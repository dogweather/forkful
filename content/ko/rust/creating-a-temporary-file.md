---
title:    "Rust: 임시 파일 생성하기"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜
C에서 임시 파일을 생성하는 것이 어려웠기 때문에 러스트에서 임시 파일을 만드는 것이 일반적입니다. 임시 파일은 일반적으로 프로그램이 실행될 때 필요한 임시 데이터 또는 캐시 파일을 저장할 때 사용됩니다. 또한 임시 파일은 프로그램간의 통신을 위한 임시적인 저장 공간으로 사용될 수도 있습니다.

## 만드는 방법
```Rust
use std::fs::File;
use std::io::prelude::*;

// 임시 파일 경로를 포함한 임시 파일 핸들을 반환합니다.
let mut tmp_file = tempfile::NamedTempFile::new().unwrap();

// 임시 파일에 쓸 데이터를 생성합니다.
let data = "Hello, world!";
println!("원본 데이터: {}", data);

// 데이터를 임시 파일에 씁니다.
tmp_file.write_all(data.as_bytes()).unwrap();

// 또는 임시 파일에서 데이터를 읽어올 수도 있습니다.
let mut buffer = String::new();
tmp_file.read_to_string(&mut buffer).unwrap();
println!("읽어온 데이터: {}", buffer);

// 임시 파일을 제거할 때는 tmp_file 핸들에 drop 함수를 호출합니다.
drop(tmp_file);
```

## 깊게 들어가기
임시 파일은 운영체제의 임시디렉토리에 자동으로 생성되기 때문에 일일이 경로를 지정할 필요가 없습니다. 또한 임시 파일을 제거할 때 자동으로 임시 파일의 참조 수를 카운트하여 모든 핸들이 제거되면 해당 파일이 자동으로 제거됩니다. 또한 임시 파일의 이름은 무작위로 생성되기 때문에 보안에도 도움이 됩니다.

## 참고하기
- [Rust 공식 문서 - Tempfile 사용하기](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust Cookbook - 임시 파일 생성하기](https://rust-lang-nursery.github.io/rust-cookbook/os/files/temp-file.html)
- [What are temporary files in rust?](https://stackoverflow.com/questions/43272955/what-are-temporary-files-in-rust)