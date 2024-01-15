---
title:                "표준 에러에 쓰기"
html_title:           "Rust: 표준 에러에 쓰기"
simple_title:         "표준 에러에 쓰기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

왜 누군가가 표준 오류에 쓰기를 하는 것에 참여할까요? 이것은 프로그래밍에서 매우 유용한 기술입니다. 보통 디버깅이나 로그 작성에 사용되며, 코드의 오류를 쉽게 찾을 수 있도록 도와줍니다.

## 하는 방법

아래는 Rust를 사용해 표준 오류에 쓰는 예제 코드입니다.

```Rust
use std::io::Write; // 표준 라이브러리에서 Write 트레이트 가져오기

fn main() {
    let mut stderr = std::io::stderr(); // 표준 오류를 가리키는 stderr 변수 생성
    writeln!(stderr, "표준 오류에 쓰기 예제"); // stderr에 문자열 쓰기
}
```

실행 결과:

`표준 오류에 쓰기 예제`

위 코드에서 `File::stderr` 함수를 사용해 표준 오류에 쓰는 것도 가능합니다.

```Rust
use std::fs::File; // 표준 라이브러리에서 File 타입 가져오기
use std::io::Write; // 표준 라이브러리에서 Write 트레이트 가져오기
use std::path::Path; // 표준 라이브러리에서 Path 타입 가져오기

fn main() {
    let stderr_path = Path::new("/dev/stderr"); // 표준 오류를 가리키는 경로 생성
    let mut file = File::create(&stderr_path).expect("파일을 생성할 수 없습니다."); // 경로를 이용해 파일 생성
    writeln!(&mut file, "다른 방법으로 표준 오류에 쓰기!"); // 파일에 문자열 쓰기
}
```

실행 결과:

`다른 방법으로 표준 오류에 쓰기!`

## 깊이 파고들기

자세한 내용은 [Rust 공식 문서의 Error Handling](https://doc.rust-lang.org/book/second-edition/ch09-00-error-handling.html) 섹션을 참조해주세요. 이 섹션에는 Error와 Panic, Result 타입 등 러스트에서 에러를 다루는 방법이 나와있습니다.

## 참고

- [The Rust Programming Language](https://www.rust-lang.org/)
- [Rust 공식 문서](https://doc.rust-lang.org)
- [Rust 커뮤니티 포럼](https://users.rust-lang.org)
- [Rust 채널 on Reddit](https://www.reddit.com/r/rust/)