---
date: 2024-01-20 18:04:17.609900-07:00
description: "How to: (\uC2DC\uC791\uD558\uB294 \uBC95) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.915777-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## How to: (시작하는 법)
```Rust
// Rust에서 새 프로젝트를 시작하는 방법:
// 1. Cargo, Rust의 패키지 매니저와 빌드 시스템을 사용합니다.
// 터미널에서 다음 명령어를 실행하세요.
cargo new my_project
cd my_project

// 2. 이제 여러분의 새 프로젝트 폴더에는 기본 파일들이 생성되었습니다.
// main.rs를 편집하여 시작하세요.
fn main() {
    println!("Hello, world!");
}

// 3. 프로젝트를 빌드하고 실행하세요.
cargo run

// 샘플 출력:
//    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
//     Running `target/debug/my_project`
// Hello, world!
```

## Deep Dive (깊이 알아보기)
Rust의 프로젝트 관리 시스템인 Cargo는 2015년에 Rust 1.0과 함께 정식 출시되었습니다. 이전에는 Rust 코드를 수동으로 컴파일하고 관리해야 했지만, Cargo는 이러한 과정을 자동화하여 개발 효율을 크게 향상시켰습니다. `cargo new` 커맨드는 새로운 프로젝트를 위한 기본 디렉터리 구조를 생성하고, 또한 `Cargo.toml` 설정 파일을 만들어 프로젝트 종속성과 설정을 관리합니다. 대안으로는 Makefile, CMake 등이 있지만, Rust 생태계 내에서는 Cargo가 표준으로 자리 잡았습니다. 구현 상세를 보면, Cargo는 Rust 프로젝트에 대한 크로스 플랫폼 빌드, 종속성 관리, 테스트 실행 기능을 제공합니다.

## See Also (더 알아보기)
- [공식 Rust 문서](https://doc.rust-lang.org/cargo/)
- [Cargo Book](https://doc.rust-lang.org/cargo/index.html)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Crates.io, Rust의 패키지 레지스트리](https://crates.io/)
