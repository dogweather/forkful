---
title:    "Rust: 텍스트 파일 읽기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것에 대한 필요성을 간단히 설명해보자면, 우리는 텍스트 파일에 포함된 정보를 다양한 방식으로 활용할 수 있습니다. 예를 들어, 우리는 텍스트 파일에 저장된 데이터를 가공하여 원하는 형식으로 출력할 수 있고, 텍스트를 분석하여 통계 정보를 도출할 수도 있습니다. 따라서 텍스트 파일을 읽는 것은 프로그래밍에서 매우 중요한 부분입니다.

## 어떻게

우리는 Rust 프로그래밍 언어를 사용하여 텍스트 파일을 읽는 방법을 알아볼 것입니다. 먼저, 텍스트 파일을 읽기 위해서는 Rust 프로그래밍 언어의 표준 라이브러리인 `std::fs` 모듈을 사용해야 합니다. 이 모듈에는 파일을 읽는 데 필요한 여러 가지 함수가 포함되어 있습니다. 예를 들어, `std::fs::read_to_string` 함수를 사용하면 텍스트 파일의 내용을 문자열로 읽어올 수 있습니다.

```rust
use std::fs;

fn main() {
    let file_content = fs::read_to_string("file.txt").expect("Unable to read file");
    println!("File content: {}", file_content);
}
```

위의 코드를 실행하면, `file.txt` 파일의 내용이 출력될 것입니다.

```
File content: This is a text file.
```

또 다른 방법은 `std::fs::File` 타입을 사용하여 파일을 열고 `std::io::BufReader` 타입을 사용하여 파일의 내용을 읽어오는 것입니다. 이 방식은 좀 더 세밀한 제어가 필요한 경우에 유용합니다.

```rust
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

fn main() {
    let file = File::open("file.txt").expect("Unable to open file");
    let mut buffer = BufReader::new(file);
    let mut contents = String::new();
    buffer.read_to_string(&mut contents).expect("Unable to read file");
    println!("File content: {}", contents);
}
```

위의 코드에서는 `read_to_string` 함수 대신 `BufReader`를 사용하여 파일의 내용을 읽어오고, `String` 타입의 변수 `contents`에 저장한 후 출력하도록 했습니다.

```
File content: This is a text file.
```

## 깊이 파고들기

텍스트 파일을 읽는 것은 단순하지만 효율적인 방식으로 처리하는 것이 중요합니다. 따라서 파일을 열기 전에 파일이 존재하는지 확인하는 것이 좋습니다. 이를 위해 우리는 `std::fs::metadata` 함수를 사용할 수 있습니다. 이 함수는 파일의 메타데이터를 반환하므로 파일의 존재 여부를 알 수 있습니다.

또한, Rust는 텍스트 파일을 읽는 것 뿐만 아니라 다양한 파일 형식을 지원합니다. 예를 들어, CSV 파일, JSON 파일, YAML 파일 등을 읽을 수 있습니다. 각 파일 형식마다 다른 라이브러리를 사용해야 하므로 필요에 따라 해당 라이브러리를 찾아 구현해야 합니다.

## 다른 글 보기

- [Rust 프로그래밍 언어 공식 문서 (한국어)](https://doc.rust-lang.org/std/fs/fn.read_to_string.html)
- [Rust 스택 오버플로우 태그](https://stackoverflow.com/questions/tagged/rust)
- [Rust 파일 처리 라이브러리 목록](https://github.com/fiji-flo/awesome-rust-kr#file-handling)