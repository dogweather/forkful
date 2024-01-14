---
title:    "Rust: 테스트 작성하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

소프트웨어 엔지니어링에서 테스트 작성은 매우 중요합니다. 코드의 일부를 변경하여 버그를 만들지 않도록 확인하는 것이 매우 중요합니다. 또한 테스트를 통해 코드를 실행하기 전에 예상하던 결과를 얻을 수 있습니다. 따라서 테스트를 작성하는 것은 코드의 안정성을 보장하는 중요한 단계입니다.

## 하우 투

Rust는 C++과 자바와 같은 다른 언어와는 다르게 Rust의 테스트 작성은 쉽고 간단합니다. 일단 테스트 파일을 만드는 것에서 시작합니다. 테스트 파일 이름은 "tests"로 끝나야 하며, "test" 함수를 이용해 함수 앞에 "tests!"를 입력하여 테스트 코드를 작성할 수 있습니다.

```Rust
    #[test]
    fn test_addition() {
        assert_eq!(2 + 2, 4);
    }
```
`#[test]`는 "test" 함수가 테스트 함수임을 선언하는 어노테이션입니다. 함수 내부에서는 `assert_eq!`를 사용하여 예상 결과와 실제 결과가 같은지를 비교합니다. 이와 같이 간단한 예제를 통해 테스트를 작성할 수 있습니다.

## 딥 다이브

Rust에서 테스트 작성의 더 깊은 부분을 이해하려면 `cargo test` 명령어를 이용할 수 있습니다. 이 명령어를 통해 테스트를 실행하고 결과를 확인할 수 있습니다. 또한 Rust에서는 `result`를 통해 테스트 결과를 분석할 수 있습니다. 이를 통해 자동화된 테스트 스위트를 작성할 수 있으며 코드를 변경하거나 리팩토링해도 테스트를 수행해 코드 안정성을 보장할 수 있습니다.

## 참고

- [Rust 공식 문서 - 테스트 작성하기](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Rust 테스트 코드 예제](https://github.com/rust-lang/rust/blob/master/src/test/run-pass/issues/issue-71846.rs)
- [도메인마다 다른 테스트 코드 - 이종원 블로그](https://blog.leocat.kr/posts/2017/04/29/unit-testing-with-rust.html)