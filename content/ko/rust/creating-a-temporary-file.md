---
title:                "Rust: 임시 파일 생성하기"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

일시적인 파일을 생성하는 것에 참여하는 이유는 무엇일까요? Rust 언어의 기능과 유틸리티에 대해 배우는 것을 좋아하거나, 다른 언어에서는 더욱 복잡하게 구현되는 작업을 간단하고 효율적으로 수행하고 싶어서일 수 있습니다.

## 어떻게

Rust는 `std::fs::File` 모듈 안에 `tempfile()` 함수를 포함하고 있습니다. 이 함수는 일시적인 파일을 생성할 수 있는 `Tempfile` 구조체를 반환합니다. 아래의 코드 예제는 이 함수를 사용하는 방법을 보여줍니다.

```Rust
use std::fs::File;
use tempfile::tempfile;

// 일시적인 파일 생성
let mut tempfile = tempfile().unwrap();
// 파일에 데이터 쓰기
tempfile.write(b"Hello, world!").unwrap();
// 파일 읽기
let mut buffer = String::new();
tempfile.read_to_string(&mut buffer).unwrap();
println!("{}", buffer); // Hello, world!
```

위의 예제에서 `tempfile()` 함수는 일시적인 파일을 생성하기 위해 `Tempfile` 구조체를 사용합니다. 파일에 데이터를 쓰고 읽을 때는 기존의 `std::fs::File` 모듈과 동일하게 사용할 수 있습니다.

## 딥 다이브

일시적인 파일을 생성하는 내부 동작을 조금 더 살펴보겠습니다. Rust는 `tempfile()` 함수를 호출할 때 `NamedTempFileOptions` 구조체를 매개변수로 받습니다. 이 구조체는 다양한 옵션을 설정할 수 있게 해줍니다. 예를 들어, 파일이 생성되는 위치, 파일의 접미사, 파일의 소유자 등을 지정할 수 있습니다. 또한 `tempfile()` 함수는 새로운 일시적인 파일을 생성하기 위해 Rust의 내장된 임시 디렉토리를 사용합니다.

## 참고 자료

- [Rust 공식 문서 - std::fs::File 모듈](https://doc.rust-lang.org/std/fs/struct.File.html)
- [tempfile Crate 문서 - tempfile() 함수](https://docs.rs/tempfile/3.1.0/tempfile/fn.tempfile.html)
- [Rust Playground에서 `tempfile()` 함수 사용하기](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=62f8810ced2719349be3dddc61df68ae)