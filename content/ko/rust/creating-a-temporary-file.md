---
title:    "Rust: 임시 파일 만들기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
임시 파일을 만드는 것에 참여하는 이유는 무엇일까요? 이 글에서는 Rust를 사용하여 임시 파일을 만드는 방법과 그 중요성에 대해 알아보겠습니다.

## 어떻게
임시 파일을 만드는 것은 파일 시스템에서 매우 일반적인 작업입니다. 이를 위해 Rust에서는 `tempfile` 라이브러리를 사용할 수 있습니다. 아래의 코드 블록을 살펴보세요.

```Rust
use tempfile::NamedTempFile;
use std::io::prelude::*;

fn main() {
    // 임시 파일 생성
    let mut tmp_file = NamedTempFile::new().expect("Cannot create temporary file");

    // 임시 파일에 쓰기
    tmp_file
        .write_all(b"Hello, world!")
        .expect("Cannot write to temporary file");

    // 임시 파일에서 읽기
    let mut contents = String::new();
    tmp_file.seek(std::io::SeekFrom::Start(0)).unwrap();
    tmp_file.read_to_string(&mut contents).unwrap();
    println!("임시 파일의 내용: {}", contents);
}
```

위의 코드는 `tempfile` 라이브러리를 사용하여 임시 파일을 생성하고, 해당 파일에 쓰고, 읽는 예제입니다. 실행하면 `Hello, world!`라는 내용을 임시 파일에서 읽어올 수 있습니다.

## Deep Dive
이제 임시 파일 생성에 대해 좀 더 깊이 알아보겠습니다. Rust에서는 `NamedTempFile`과 `TempDir` 두 가지 방법으로 임시 파일을 생성할 수 있습니다. `NamedTempFile`은 이름이 있는 임시 파일이며, 운영 체제에 영향을 받지 않는 고유한 이름을 가지고 있습니다. 반면 `TempDir`은 이름이 없는 임시 디렉토리를 생성하게 됩니다.

또한 `tempfile` 라이브러리를 사용하여 생성된 임시 파일은 프로세스가 종료되면 자동으로 삭제되기 때문에 메모리 부족 현상을 방지할 수 있습니다.

## 참조
- [tempfile 라이브러리 공식 문서](https://docs.rs/tempfile/)
- [Rust 표준 라이브러리의 `tempfile` 모듈](https://doc.rust-lang.org/std/fs/struct.OpenOptions.html)
- [Rust 임시 파일을 사용하여 데이터베이스를 테스팅하는 예제](https://github.com/psmarcin/tempfile)
- [Rust 임시 파일을 활용한 예제 프로젝트](https://github.com/hallazzang/rust-tempfile-how-to)

## 더 알아보기
위의 예제 코드에서 `tempfile` 라이브러리를 사용하여 임시 파일을 생성하고 다룬 방법 이외에도 다른 방법들이 있습니다. 더 자세한 내용은 [tempfile 라이브러리 공식 문서](https://docs.rs/tempfile/)를 참고해보세요.