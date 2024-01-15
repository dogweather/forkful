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

## 왜

텍스트 파일을 읽는 방법을 배우는 것은 프로그래밍에서 중요한 기술 중 하나입니다. 이를 통해 여러분은 파일을 조작하고 내용을 분석할 수 있으며, 더 나은 애플리케이션을 개발하는 데 도움이 될 수 있습니다.

## 사용 방법

텍스트 파일을 읽는 가장 간단한 방법은 `std::fs` 모듈의 `read_to_string()` 함수를 사용하는 것입니다. 이 함수는 파일 이름을 매개변수로 받고, 파일의 내용을 문자열로 반환합니다.

```Rust
use std::fs;

let file_content = fs::read_to_string("sample.txt")
    .expect("Failed to read file");

println!("{}", file_content);
```

위 예제는 "sample.txt" 파일을 읽고, 파일의 내용을 콘솔에 출력하는 간단한 예제입니다.

## 깊이 있는 설명

위의 예제에서 `expect()` 함수는 에러 처리를 위해 사용되었습니다. 이 함수는 파일을 읽을 수 없는 경우, 프로그램을 중단하고 에러 메시지를 출력합니다. 더 복잡한 애플리케이션에서는 이 에러를 처리하는 코드를 추가해야합니다.

또한, 파일의 내용을 읽는 방법은 `read_to_string()` 함수뿐만 아니라 다른 메서드들도 존재합니다. 예를 들어, `read()` 함수는 파일의 내용을 바이트 형태로 반환합니다.

파일을 읽는 방법에 대한 더 자세한 정보는 [Rust 공식 문서](https://doc.rust-lang.org/std/fs/fn.read_to_string.html)를 참고하시기 바랍니다.

## 관련 링크

- [Rust 공식 문서](https://www.rust-lang.org/ko)
- [격식을 버리고 쓰는 Rust (Rust for casual speakers)](https://github.com/rust-lang/rfcs/blob/master/text/2632-casual-rust.md)
- [Rust 커뮤니티 포럼](https://users.rust-lang.org/)