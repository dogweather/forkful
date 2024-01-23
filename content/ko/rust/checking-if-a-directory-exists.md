---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:58:54.484754-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)

디렉토리가 존재하는지 확인하는 것은 파일 시스템에 특정 경로의 폴더가 있는지 알아보는 과정입니다. 프로그래머들은 이 작업을 통해 파일 작업 오류를 방지하고, 필요할 경우 디렉토리를 만들기 전에 미리 검사합니다.

## How to: (방법:)

Rust에서는 `std::path::Path`와 `std::fs` 모듈을 사용하여 디렉토리가 존재하는지 확인할 수 있습니다.

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/some/directory");

    if path.exists() {
        println!("{:?} directory exists!", path);
    } else {
        println!("{:?} directory does not exist.", path);
    }
}
```

만약 `/some/directory`가 존재한다면 콘솔에는 다음과 같은 메시지가 표시됩니다.

```
"/some/directory" directory exists!
```

존재하지 않는다면 다음과 같이 표시됩니다.

```
"/some/directory" directory does not exist.
```

## Deep Dive (심층 탐구)

과거 언어들에서는 디렉토리가 존재하는지 여부를 확인하기 위해 다양한 방법을 사용했습니다. 예를 들어, C에서는 `stat` 함수를 사용했고, Python에서는 `os.path.exists()` 함수를 사용했습니다. Rust에서는 표준 라이브러리에 이러한 기능을 내장하고 있어, 보다 Rust스러운 API를 제공합니다.

`std::fs::metadata()` 함수를 사용하면 `Path`에 대한 메타데이터를 얻을 수 있고, 이는 존재 여부뿐만 아니라 디렉토리인지, 파일인지 등의 정보도 포함하고 있습니다. 그러나 간단히 존재 여부만 확인하고자 할 때는 `Path`의 `exists` 메서드를 사용하는 것이 더 직관적이고 간결합니다.

대안으로는 `std::fs::read_dir()`을 사용해 디렉토리의 내용물을 읽으려 시도하고, `Result` 타입을 검사하는 방법도 있습니다. 만약 결과가 `Err`이면 디렉토리가 존재하지 않거나 접근할 수 없다는 것을 의미할 수 있습니다.

고려해야 할 또 다른 사항은 경쟁 조건(race condition)입니다. 존재 여부를 확인한 후에 실제 작업을 수행하기 전에 디렉토리 상태가 변경될 수 있습니다. 실제 파일 작업 전에는 추가적인 오류 처리가 필요할 수 있습니다.

## See Also (참고 자료)

- [The Rust Programming Language - std::path](https://doc.rust-lang.org/std/path/)
- [The Rust Programming Language - std::fs](https://doc.rust-lang.org/std/fs/)
- [Rust by Example - Filesystem Operations](https://doc.rust-lang.org/rust-by-example/std_misc/fs.html)
- [Rust Book - Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
