---
title:                "텍스트 파일 읽기"
html_title:           "Rust: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

텍스트 파일을 읽는 것은 간단히 말해, 컴퓨터에서 원하는 파일의 내용을 읽어오는 것입니다. 프로그래머들이 이것을 하는 이유는 다양합니다. 예를 들어, 데이터 분석을 위해 파일의 내용을 읽어오거나, 인터넷에서 데이터를 가져와서 저장하기 위해 사용할 수도 있습니다.

## 사용 방법:

Rust에서 텍스트 파일을 읽는 방법은 매우 간단합니다. 먼저 `std::fs` 라이브러리를 import해야 합니다. 그 다음, `File::open()` 함수를 사용하여 파일을 열고, `BufReader::new()`로 버퍼를 생성한 다음, `lines()` 함수를 사용하여 파일의 각 줄을 읽습니다.

```Rust
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

fn main() {
    let file = File::open("file.txt").expect("파일이 존재하지 않습니다.");
    let reader = BufReader::new(file);

    for line in reader.lines() {
        println!("{}", line.unwrap());
    }
}
```

위 코드는 현재 디렉토리에 있는 `file.txt` 파일을 열어서 각 줄을 출력합니다. `expect()` 함수는 파일이 존재하지 않을 경우에 발생할 수 있는 오류를 처리하기 위해 사용됩니다.

## 깊이 있는 내용:

텍스트 파일을 읽는 것은 오래된 프로그래밍 기법 중 하나입니다. 이전에는 텍스트 파일을 사용해서 데이터를 저장하는 것이 거의 유일한 방법이었기 때문입니다. 현재는 더 많은 선택지가 있지만, 여전히 텍스트 파일을 사용하는 경우도 많습니다.

또한, 텍스트 파일을 읽을 때는 개행 문자를 처리하는 것이 중요합니다. 각 운영 체제마다 개행 문자의 종류가 다르기 때문에, 이를 유의해야 합니다.

텍스트 파일을 읽는 과정에서 다양한 오류가 발생할 수 있습니다. 따라서 오류 처리에 대해서도 꼼꼼하게 고려해야 합니다.

## 참고 자료:

- [Rust 공식 문서](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust Cookbook - 파일 읽기](https://rust-lang-nursery.github.io/rust-cookbook/file/read-write.html)