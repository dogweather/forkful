---
title:                "Rust: 프로그래밍에서의 테스트 작성"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-tests.md"
---

{{< edit_this_page >}}

Rust 프로그래밍을 하면서 다른 언어에 비해 항상 강조되는 부분이 있습니다. 그것은 바로 테스트입니다. Rust에서 테스트를 작성하는 것은 중요합니다. 왜냐하면 기능을 검증하고 약한 부분을 찾는 것에 도움이 되기 때문입니다.

## 왜?

Rust는 컴파일 단계에서 많은 오류를 잡아내기 때문에 런타임에서 발생할 수 있는 오류를 최소화할 수 있습니다. 하지만 실제로 코드가 제대로 동작하는지는 컴파일러에서는 확인할 수 없습니다. 따라서 테스트를 통해 코드를 더욱 견고하게 만들 수 있습니다.

## 어떻게?

Rust에서 테스트를 작성하는 방법을 알아보겠습니다. 테스트는 보통 `tests` 디렉토리에 작성하며, 모듈로 구성됩니다.`tests` 디렉토리 안에 `mod.rs` 파일을 만들어서 다음과 같은 코드를 작성합니다.

```Rust
mod example_test {
    #[test]
    fn test_example() {
        assert_eq!(2 + 2, 4);
    }
}
```

테스트 함수는 `test` 어노테이션으로 시작하며, 내부에서는 내장된 `assert` 매크로를 사용하여 검증을 할 수 있습니다. 예제에서는 2 + 2가 4인지 검증하는 테스트를 작성하였습니다.

`Rust` 디렉토리 안에 있는 `Cargo.toml` 파일에 다음과 같이 `[package]` 아래에 `test` 키워드를 추가해줍니다.

```Rust
[package]
name = "example"
version = "0.1.0"
authors = ["Your Name <your.email@example.com>"]
edition = "2018"

[dependencies]

[package.metadata.docs.rs]
features = ["docs"]

[package.metadata.ferrous-systems-unstable-macro-rfcs]
enabled = false

# Add this line
test = true
```

이제 `cargo test` 명령어를 실행하여 테스트를 실행할 수 있습니다. 테스트 결과는 터미널에 출력되며, 성공한 테스트는 `ok`로, 실패한 테스트는 `FAILED`로 표시됩니다.

## 심층 분석

테스트를 작성할 때는 테스트를 진행하는 동안 생길 수 있는 모든 경우를 고려해야 합니다. Rust에서는 이를 위해 여러 가지 매크로를 제공합니다. 예를 들어 `assert_eq!` 매크로는 두 개의 값이 동일한지를 검증하며, `assert_ne!` 매크로는 두 개의 값이 다른지를 검증합니다.

테스트 코드에서는 `Result` 타입을 사용하여 함수의 반환 값이 예상한 값과 일치하는지를 검증할 수도 있습니다. 이를 통해 코드의 에러 핸들링을 검증할 수 있습니다.

## 이것도 참고하세요

- [Rust 공식 문서: Testing](https://doc.rust-lang.org/stable/book/ch11-00-testing.html)
- [Rust by Example: Testing](https://doc.rust-lang.org/rust-by-example/testing.html)
- [Rust Test](https://rust-lang-nursery.github.io/rust-cookbook/testing/integration_tests.html)

## 참고 링크

https://doc.rust-lang.org/book/title-page.html
https://doc.rust-lang.org/rust-by-example/testing.html
https://rust-lang-nursery.github.io/rust-cookbook/testing/integration_tests.html