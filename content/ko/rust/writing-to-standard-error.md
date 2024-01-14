---
title:                "Rust: 표준 에러 쓰기"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍을 하는 사람이라면 누구나 생각해볼 수 있는 질문 중 하나는 "도대체 왜 에러 메시지가 중요한가?"입니다. 에러 메시지는 우리가 코딩을 할 때는 꼭 필요한 것이지만, 우리가 원하는 대로 프로그램이 동작하지 않을 때 우리에게 무슨 일이 벌어졌는지 알려줍니다. 이는 앞으로 발생할 문제를 해결하는 데 도움이 됩니다.

## 사용 방법

우리는 Rust 러스트 프로그래밍 언어로 코딩을 할 때 종종 `standard error`에 메시지를 쓸 필요가 있습니다.

우선 우리는 `use std::io::{self, Write};`를 추가해줍니다. 그리고 `main` 함수 안에 아래 코드를 삽입합니다:

```Rust
let stderr = io::stderr();
let mut handle = stderr.lock();
let _ = handle.write(b"에러 메시지를 적어주세요!"); // 이 부분을 우리가 원하는 에러 메시지로 바꿔줍니다
```

위 코드를 실행하면 우리가 원하는 에러 메시지가 `standard error`로 출력될 것입니다.

## 깊이 파고들기

1. [`std::io::Write`](https://doc.rust-lang.org/std/io/trait.Write.html): Rust에서 `Write` trait의 공식 문서입니다.
2. [Rust 에러 핸들링(github repo)](https://github.com/sgrif/rustando/blob/master/src/error_handling.rs): 에러 핸들링 관련 예제 코드를 모아놓은 깃허브 레포지토리입니다.

## 참고자료

1. [Rust 공식 문서 - 에러 핸들링](https://doc.rust-lang.org/stable/book/ch09-00-error-handling.html): Rust의 에러 핸들링에 대한 자세한 설명이 담겨있는 공식 문서입니다.
2. [Rust Programming Language (youtube playlist)](https://www.youtube.com/playlist?list=PLVvjrrRCBy2JSHf9tGxGKJ-bYAN_uDCUL): 러스트 프로그래밍 언어에 관한 유튜브 강의입니다.
3. [Rust Forum](https://users.rust-lang.org/): 러스트 프로그래밍 언어에 관한 질문과 답변을 공유하는 포럼입니다.