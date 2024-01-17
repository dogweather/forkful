---
title:                "텍스트 파일 작성하기"
html_title:           "Rust: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 파일 쓰기는 개발자들이 컴퓨터에 텍스트를 저장하기 위해 사용하는 방법입니다. 이를 통해 프로그래머들은 생성된 정보를 나중에 다시 사용할 수 있습니다.

## 하는 방법:

```Rust
// Create a new text file
use std::fs::File;
let mut file = File::create("example.txt").expect("Failed to create file");

// Write text to the file
use std::io::Write;
file.write_all(b"Hello, world!").expect("Failed to write to file");

// Read text from the file
use std::fs::File;
use std::io::Read;
let mut file = File::open("example.txt").expect("Failed to open file");
let mut contents = String::new();
file.read_to_string(&mut contents).expect("Failed to read from file");
assert_eq!(contents, "Hello, world!");
```

## 깊이 파고들기:

텍스트 파일 쓰기는 우리가 사용하는 모든 소프트웨어에 필수적인 기능입니다. 이전에는 다양한 방법으로 텍스트를 저장했지만, 현재는 크로스 플랫폼 지원이 가능한 텍스트 파일 쓰기 방법이 우리에게 제공됩니다.

## 관련 자료:

- [Rust 표준 라이브러리](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust 책](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html)