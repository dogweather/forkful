---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

---

## 무엇 & 왜?

새 프로젝트를 시작하는 것은 컴퓨터 프로그래밍에서 새로운 문제나 필요에 대응하는 소프트웨어를 개발하기 위한 절차입니다. 이는 솔루션을 변경하거나 향상시키기 위한 첫 단계입니다.

## 어떻게:

먼저, 새로운 Rust 프로젝트를 생성해 보겠습니다. 터미널에서, 다음 코드를 입력합니다:

```Rust
$ cargo new hello_world
$ cd hello_world
```

이 명령어는 `hello_world`라는 새로운 Rust 프로젝트를 생성합니다. 이제 해당 프로젝트의 메인 파일을 살펴보겠습니다.

```Rust
fn main() {
    println!("Hello, world!");
}
```

이 코드를 실행하면, 출력 결과는 다음과 같습니다.

```Rust
$ cargo run
  Compiling hello_world v0.1.0 (/path/to/hello_world)
   Running `target/debug/hello_world`
Hello, world!
```

## 깊이 파보기

Rust는 애초에 안전하고 동시성 프로그래밍을 간소화하기 위해 설계되었습니다. 새 프로젝트를 시작하면서 이러한 장점을 이용하여 효율적인 프로그램을 개발할 수 있습니다.

Rust의 대체 옵션으로는 C/C++, Python 등 다양한 언어가 있습니다. 그러나 Rust는 메모리 안전성과 같은 강력한 특징을 가지고 있습니다.

새 프로젝트를 시작할 때, `cargo new` 명령을 수행하면 Cargo는 기본 바이너리 프로젝트 또는 라이브러리 프로젝트를 생성합니다. 이는 우리가 앞에서 보았듯이, 기본적인 "Hello, World!" 프로그램도 포함합니다.

## 참고자료

앞으로 Rust 학습을 계속하는 데에 도움이 될 몇 가지 리소스를 소개합니다:

1. [Rust 공식 문서](https://doc.rust-lang.org/book/)
2. [Rust 한국어 번역 문서](https://rinthel.github.io/rust-lang-book-ko/)
3. [Rust에 익숙해지기](https://cheats.rs/)
4. [Cargo 공식 가이드](https://doc.rust-lang.org/cargo/guide/)

이 리소스들을 통해 Rust에서 새 프로젝트를 만드는 데에 필요한 더 많은 디테일과 기능을 알아볼 수 있습니다.