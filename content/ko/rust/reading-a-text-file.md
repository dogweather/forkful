---
title:    "Rust: 텍스트 파일 읽기"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜?

텍스트 파일을 읽는 방법에 대해 궁금하지만 이를 배우는 데 어려움을 겪고 있는 분들을 위해 이 포스트를 작성하였습니다. 우리는 이 블로그 포스트를 통해 Rust 프로그래밍 언어를 사용하여 텍스트 파일을 읽는 방법을 알아보도록 하겠습니다.

## 방법

우선, Rust 언어를 사용하여 파일을 읽는 가장 간단한 방법은 `std::fs::File`을 사용하는 것입니다. 먼저, `File::open()` 함수를 사용하여 파일을 열고, `read_to_string()` 메서드를 사용하여 파일의 내용을 문자열로 읽어올 수 있습니다. 아래는 실제 코드와 예상되는 출력입니다.

```rust
use std::fs::File;
use std::io::Read;

fn main() {
    let mut file = File::open("sample.txt").expect("파일을 열 수 없습니다.");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("파일을 읽을 수 없습니다.");
    println!("{}", contents);
}
```

```
Hello, world!
안녕하세요!
```

위의 코드를 간단히 설명하면, `sample.txt` 파일을 열고 해당 파일의 내용을 `contents` 문자열 변수에 저장한 뒤, 해당 문자열을 화면에 출력하는 것입니다.

## 깊이 파고들기

파일을 읽는 예제를 통해 실제로는 Rust에서 텍스트 파일을 읽는 과정이 어떻게 일어나는지 알아보겠습니다. 우선 `File::open()` 함수는 `Result` 타입을 반환하며, 이는 성공적으로 파일을 열었을 때 `Ok` 결과를, 실패했을 때 `Err` 결과를 반환합니다. 그리고 `read_to_string()` 메서드는 파일의 내용을 읽은 뒤 해당 문자열로 반환하는 `Result` 타입을 반환합니다. 따라서 위의 예제 코드에서는 `파일을 열 수 없습니다.` 또는 `파일을 읽을 수 없습니다.`라는 에러 메시지가 출력될 수 있습니다.

이처럼 Rust에서 파일을 읽는 과정은 간단하지만 안전성을 위해 몇 가지 검사를 해야 합니다. 따라서 파일을 열고 읽는 과정에서 발생할 수 있는 여러 가지 에러 상황을 처리하는 것이 중요합니다.

## 더 알아보기

지금까지 파일을 읽는 간단한 예제를 통해 Rust에서 파일 처리를 해본 것은 좋은 시작입니다. 하지만 Rust에서는 더 많은 방법으로 파일을 처리할 수 있습니다. 예를 들어, `std::io` 모듈을 통해 파일의 특정 위치에서 부터 일정한 크기만큼 데이터를 읽거나 쓸 수 있습니다. 또 다른 흥미로운 Rust 라이브러리인 `serde`를 사용하면 파일을 파싱하거나 직렬화하는 것도 가능합니다.

## 더 알아보기

- [Rust 공식 문서: 파일 처리](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)
- [Rust문법 - file 처리](https://gompangs.tistory.com/entry/Rust%EB%AC%B8%EB%B2%95-file-%EC%B2%98%EB%A6%AC)
- [Rust를 사용한 파일 입출력](https://blog.itplatoon.com/2021/06/10/rust%EB%A5%BC-%EC%82%AC%EC%9A%A9%ED%95%9C-%ED%8C%8C%EC%9D%BC-%EC%9E%85%