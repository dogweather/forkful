---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 임시 파일 생성하기 : Rust를 활용한 방법

## 무엇이고 왜 필요할까?
임시 파일은 디스크에 잠시 저장되는, 일시적인 정보의 저장소입니다. 이는 대용량 데이터 처리, 데이터 변환, 서버 간 정보 전송 등에서 중요하게 활용됩니다.

## 이렇게 하세요:
Rust는 `tempfile`이라는 crate를 제공하여 임시 파일을 생성하고 관리하는 것을 쉽게 도와줍니다.
```Rust
use std::io::{Write, Seek, SeekFrom};
use tempfile::tempfile;

fn main() {
    let mut file = tempfile().unwrap();

    write!(file, "Hello, world!").unwrap();
    file.seek(SeekFrom::Start(0)).unwrap();

    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    assert_eq!(&buf, "Hello, world!");
}
```
중요한 점은 `tempfile().unwrap()`가 임시 파일 인스턴스를 반환하고, 이를 사용해 파일에 쓰고 읽는 데 사용할 수 있다는 점입니다. 이렇게 해서 "Hello, world!" 문자열을 임시 파일에 쓴 다음 읽어와서 여러분이 쓴 그대로인지 확인합니다.

## 깊은 탐구
1. **역사적 문맥**: `tempfile`의 경우 Unix, Linux 커맨드라인 아이디어에서 비롯되었습니다. 이는 시스템에서 일시적 파일을 생성하고 관리하는 메커니즘을 제공합니다. 
2. **대안**: `tempdir` crate을 사용하면 임시 디렉토리를 생성하고 관리할 수 있습니다. 디렉토리를 사용하면 여러 파일을 묶어서 함께 처리할 수 있습니다. 
3. **구현 세부 사항**: tempfile는 디스크에 쓰는 대신 운영 체제의 파일 핸들러에 직접 쓰여집니다. 디스크에 접근하는 것보다 빠르고 효율적입니다. 

## 참조한 자료
* tempfile crate documentation : [https://docs.rs/tempfile/3.0.8/tempfile/](https://docs.rs/tempfile/3.0.8/tempfile/)
* Rust official documentation : [https://doc.rust-lang.org/stable/std/](https://doc.rust-lang.org/stable/std/)
* How to work with temporary Files and Directories in Rust : 
[https://www.tutorialspoint.com/how-to-work-with-temporary-files-and-directories-in-rust](https://www.tutorialspoint.com/how-to-work-with-temporary-files-and-directories-in-rust)