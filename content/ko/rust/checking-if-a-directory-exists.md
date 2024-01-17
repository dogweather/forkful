---
title:                "디렉토리가 있는지 확인하기"
html_title:           "Rust: 디렉토리가 있는지 확인하기"
simple_title:         "디렉토리가 있는지 확인하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Rust에서 폴더의 존재 여부 확인하기 

## 무엇을 & 왜?

여러분이 프로그래머라면 아마도 프로그램이 실행될 때 특정한 폴더가 존재하는지 확인하고 싶을 때가 있을 것 입니다. 이는 매우 중요한 일이며, 코드가 제대로 작동하기 위해서는 정확한 폴더 경로를 알아야 하기 때문입니다. 따라서 폴더의 존재 여부를 확인하는 것은 매우 중요한 일이 될 수 있습니다.

## 방법:

먼저, Rust에서는 `std::fs` 모듈에서 `Path` 구조체를 사용하여 폴더의 존재 여부를 확인할 수 있습니다. 이를 위해서는 `PathBuf` 구조체를 이용하여 해당 폴더의 경로를 지정하고, `exists()` 메서드를 사용하여 그 폴더가 존재하는지 여부를 확인할 수 있습니다. 예를 들어서, 다음과 같은 코드를 작성할 수 있습니다:

```Rust 
use std::fs::PathBuf;

let path = PathBuf::from("my_folder");
if path.exists() {
    println!("폴더가 존재합니다.");
} else {
    println!("폴더가 존재하지 않습니다.");
}
```

위의 코드에서는 "my_folder"라는 폴더가 현재 디렉토리에 존재하는지 확인하고, 그에 따라 적절한 메시지를 출력합니다.

## 깊이 보기:

이 기능은 Rust 1.16 버전에서 도입되었으며, 이전 버전에서는 `std::fs` 모듈의 `metadata()` 메서드를 사용하여 폴더가 존재하는지 확인했습니다. 그러나 이 방법은 폴더가 존재하는지 여부를 확인하는 것 외에도 여러 가지 다른 메타데이터를 얻어오기 때문에 비효율적이라는 단점이 있었습니다.

`Path` 구조체를 사용하면 더욱 간편하게 폴더의 존재 여부를 확인할 수 있으며, 파일의 존재 여부를 확인하는 것과 동일한 방식으로 사용할 수 있습니다.

## 관련 자료:

- [Rust 공식 문서: PathBuf 구조체](https://doc.rust-lang.org/std/path/struct.PathBuf.html)
- [Rust 공식 문서: 파일 시스템 작업하기](https://doc.rust-lang.org/std/fs/index.html)
- [Rust 공식 문서: metadata() 메서드](https://doc.rust-lang.org/std/fs/fn.metadata.html)