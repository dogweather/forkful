---
title:    "Rust: 텍스트 파일 작성하기"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일 작성에 참여하는 이유는 이해하기 쉽고 간단한 형식으로 정보를 저장하고 공유하기 위해서입니다.

## 작성 방법

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::create("example.txt")
        .expect("파일을 만들지 못했습니다.");
    let message = "안녕하세요, 여러분!";
    file.write_all(message.as_bytes())
        .expect("파일에 쓰지 못했습니다.");
}
```
위의 예제 코드는 Rust 언어를 사용하여 새로운 텍스트 파일을 만드는 방법을 보여줍니다. 파일을 만들고, 텍스트를 써서 저장하는 과정을 간단하게 나타내었습니다. 

## 깊게 들어가기

만들어진 파일을 열어보면 텍스트가 제대로 저장되어 있음을 확인할 수 있습니다. 이번에는 좀 더 깊게 들어가서, 파일에 추가적인 내용을 쓰는 방법을 알아보겠습니다.

```Rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("example.txt")
        .expect("파일을 열지 못했습니다.");
    let message = "\n만나서 반가워요!";
    file.write_all(message.as_bytes())
        .expect("파일에 쓰지 못했습니다.");
}
```

위의 예제 코드는 기존에 있던 파일을 열어서 내용을 추가하는 과정을 보여줍니다. 기존 파일에 `OpenOptions`를 사용하여 쓰기 권한과 추가 모드를 설정하고, 새로운 메시지를 추가하는 것을 확인할 수 있습니다. 

## See Also

- [Rust Language 공식 사이트](https://www.rust-lang.org/ko/)
- [Rust 프로그래밍 언어 문서](https://doc.rust-lang.org/book/ko/)