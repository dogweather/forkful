---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
테스트 작성은 코드가 예상대로 작동하는지 확인하는 과정입니다. 프로그래머들은 버그를 미리 찾아내고, 코드 변경에 따른 위험을 줄이기 위해 테스트를 합니다.

## How to: (방법)
```Rust
// src/lib.rs에 테스트 함수 정의
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
    
    #[test]
    fn it_fails() {
        assert_eq!(3 + 3, 9);
    }
}

// 터미널에서 `cargo test`로 실행
```
테스트 실행 결과:
```
running 2 tests
test tests::it_works ... ok
test tests::it_fails ... FAILED

failures:

---- tests::it_fails stdout ----
thread 'tests::it_fails' panicked at 'assertion failed: `(left == right)`
  left: `6`,
 right: `9`', src/lib.rs:10:9
```

## Deep Dive (깊이있는 탐색)
테스트는 소프트웨어 개발 초기부터 중요한 부분이었으며, Rust에서도 `cargo test` 커맨드를 통해 손쉽게 테스트할 수 있습니다. 대안으로는 주석으로 코드를 확인하는 방법도 있지만 자동화된 테스트가 더 일반적입니다. 내부적으로 Rust는 테스트 함수에 쓰레드를 할당하고 병렬로 실행할 수 있어 테스트 성능을 최적화합니다.

## See Also (함께 보기)
- [Rust 공식 문서 중 테스트 가이드](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [The Rust Programming Language (Book) – 파트 11 – 테스트](https://rinthel.github.io/rust-lang-book-ko/ch11-00-testing.html) (한국어 번역)
- [`assert!` 매크로 사용법](https://doc.rust-lang.org/std/macro.assert.html)