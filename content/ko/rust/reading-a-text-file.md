---
title:                "Rust: 텍스트 파일 읽기"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜 파일을 읽는가?

텍스트 파일을 읽는 것은 프로그래밍의 기본 중 하나입니다. 파일을 읽는 것은 프로그램에서 중요한 데이터를 쉽게 가져오는 방법입니다. 또한 파일을 읽는 것은 다른 컴퓨터 프로그램과의 상호 작용을 가능하게 합니다. 따라서 파일을 읽는 것은 프로그래밍에서 필수적입니다.

## 방법

Rust에서 파일을 읽는 것은 매우 간단합니다. 먼저 ```file.rs```라는 새로운 파일을 만들고 ```main()``` 함수를 추가합니다. 그리고 다음과 같은 코드를 입력합니다.

```
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("text.txt").expect("파일을 열 수 없습니다.");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("파일을 읽을 수 없습니다.");

    println!("{}", contents);
}
```

이 코드는 ```text.txt```이라는 파일을 열고 그 내용을 읽어들인 후, 출력하는 예제입니다. 

## 깊이 들어가기

Rust에서 파일을 읽는 것은 기본적으로 파일을 열고 그 내용을 메모리에 로드하여 다양한 작업을 할 수 있게 만듭니다. 위의 예제에서는 파일의 내용을 문자열로 읽어들인 후 출력하였지만, 실제로는 파일의 내용을 사용하여 다양한 작업을 할 수 있습니다. Rust는 파일의 내용을 다루기 위한 다양한 라이브러리를 제공하므로, 더 깊이 들어가서 이를 살펴보는 것도 좋은 방법입니다.

## 참고자료

- [Rust 문서 - 파일 입출력](https://doc.rust-lang.org/std/fs/struct.File.html)
- [여러 가지 Rust의 파일 입출력 예제](https://www.tutorialspoint.com/rust/rust_file_io.htm)
- [Rust Cookbook - 파일 입출력](https://rust-lang-nursery.github.io/rust-cookbook/file/read-write.html)