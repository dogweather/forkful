---
title:    "Rust: 디렉토리가 존재하는지 확인하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
디렉토리가 존재하는지 확인하는 것은 중요합니다. 파일 시스템에서 파일 또는 디렉토리를 처리할 때, 해당 파일 또는 디렉토리가 존재하는지 여부를 먼저 확인해야 합니다. 따라서 디렉토리의 존재 여부를 확인하는 것은 프로그래밍에서 필수적인 부분입니다.

## 어떻게
Rust로 디렉토리의 존재 여부를 확인하는 방법은 매우 간단합니다. 먼저, `std::path::Path` 구조체를 사용하여 디렉토리의 경로를 정의해야 합니다. 그런 다음 `std::fs::metadata()` 함수를 사용하여 해당 파일이나 디렉토리의 메타데이터를 가져옵니다. 마지막으로, `std::fs::Metadata::is_dir()` 메서드를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 아래의 코드 예제를 참고하세요.

```rust
use std::path::Path;
use std::fs;

fn main() {
    let path = Path::new("/home/user/documents");
    let metadata = fs::metadata(path).expect("Failed to get metadata.");

    if metadata.is_dir() {
        println!("The directory exists.");
    } else {
        println!("The directory does not exist.");
    }
}
```

위의 예제 코드를 실행하면, `documents` 디렉토리가 존재하는 경우 "The directory exists."가 출력되고, 디렉토리가 존재하지 않을 경우 "The directory does not exist."가 출력됩니다.

## 깊게 파헤치기
Rust에서 디렉토리를 확인하는 방법에 대해 더 자세히 알고 싶다면, `std::fs::metadata()` 함수의 동작 방식을 살펴볼 수 있습니다. 해당 함수는 `std::fs::Metadata` 구조체를 반환하며, 이 구조체는 해당 파일이나 디렉토리의 메타데이터를 나타냅니다. 메타데이터에는 파일의 크기, 수정일자, 권한 등의 정보가 포함되어 있습니다. 따라서 `is_dir()` 메서드를 호출하면 디렉토리인지 아닌지 여부를 알 수 있습니다.

## 이것도 보세요
- [Rust 공식 문서 (한국어)](https://doc.rust-lang.org/ko/)
- [Rust 튜토리얼 (한국어)](https://rinthel.github.io/rust-lang-book-ko/)