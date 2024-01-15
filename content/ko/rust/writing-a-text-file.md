---
title:                "텍스트 파일 작성하기"
html_title:           "Rust: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?

텍스트 파일을 작성하는 것은 많은 이유가 있을 수 있습니다. 예를 들어, 작업 중인 프로젝트의 문서를 작성할 때나 개인적인 노트를 남기기 위해 사용할 수 있습니다. 또는 블로그나 웹사이트에 게시할 글을 작성할 때도 사용할 수 있습니다. 어떤 이유든지, 텍스트 파일은 우리가 일상적으로 사용하는 매우 유용한 도구입니다.

## 작성하는 방법

어떻게 텍스트 파일을 작성할 수 있을까요? 다행히도, Rust는 강력한 표준 라이브러리를 제공하여 텍스트 파일을 쉽게 작성할 수 있도록 해줍니다. 먼저, `File` 구조체를 사용하여 쓰기 모드로 파일을 열어야 합니다. 그런 다음, `write_all` 메서드를 사용하여 원하는 텍스트를 파일에 쓸 수 있습니다. 아래는 간단한 예제 코드입니다.

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    // 쓰기 모드로 파일을 열기
    let mut file = File::create("hello.txt").expect("파일 생성에 실패하였습니다.");

    // 파일에 텍스트 작성
    file.write_all(b"안녕하세요, Rust!").expect("파일 쓰기에 실패하였습니다.");
}
```

위 코드를 실행하면 "hello.txt"라는 파일이 생성되고 해당 파일 안에 "안녕하세요, Rust!"라는 텍스트가 작성됩니다.

## 깊게 들어가보기

파일을 작성할 때 파일을 만들기 위해 `File::create` 메서드를 사용했습니다. 이 메서드는 새로운 파일을 생성할 때 사용하며, 이미 존재하는 파일을 열고 수정하고자 할 때는 `File::open` 메서드를 사용합니다. 또한, 파일을 열 때는 반드시 `expect` 메서드를 사용하여 오류를 처리해주어야 합니다.

또한, 텍스트를 파일에 작성할 때 `write_all` 메서드를 사용했습니다. 이 메서드는 파일에 바이트를 그대로 작성하기 때문에 `b""`를 사용하여 해당 바이트가 아닌 문자열을 전달해주어야 합니다.

## 관련 자료

* [Rust 공식 문서](https://doc.rust-lang.org/std/fs/struct.File.html)
* [Rust Cookbook에서 파일 작성하기 예제](https://rust-lang-nursery.github.io/rust-cookbook/file/handle.html#writing-to-a-file)