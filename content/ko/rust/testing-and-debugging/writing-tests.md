---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:09.066618-07:00
description: "\uC5B4\uB5BB\uAC8C: Rust\uC758 \uB0B4\uC7A5 \uD14C\uC2A4\uD2B8 \uD504\
  \uB808\uC784\uC6CC\uD06C\uB294 \uC678\uBD80 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00\
  \ \uD544\uC694 \uC5C6\uC774 \uB2E8\uC704, \uD1B5\uD569 \uBC0F \uBB38\uC11C \uD14C\
  \uC2A4\uD2B8\uB97C \uC9C0\uC6D0\uD569\uB2C8\uB2E4. \uD14C\uC2A4\uD2B8\uB294 `#[test]`\uB85C\
  \ \uC8FC\uC11D \uCC98\uB9AC\uB418\uBA70, \uC774\uC640 \uAC19\uC774 \uC8FC\uC11D\
  \ \uCC98\uB9AC\uB41C \uBAA8\uB4E0 \uD568\uC218\uB294 \uD14C\uC2A4\uD2B8\uB85C \uCEF4\
  \uD30C\uC77C\uB429\uB2C8\uB2E4. \uD14C\uC2A4\uD2B8\uD558\uB294 \uBAA8\uB4C8 \uC548\
  \uC5D0 `tests` \uD558\uC704 \uBAA8\uB4C8\uC744\u2026"
lastmod: '2024-03-13T22:44:54.919748-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC758 \uB0B4\uC7A5 \uD14C\uC2A4\uD2B8 \uD504\uB808\uC784\uC6CC\uD06C\
  \uB294 \uC678\uBD80 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\uC694 \uC5C6\uC774\
  \ \uB2E8\uC704, \uD1B5\uD569 \uBC0F \uBB38\uC11C \uD14C\uC2A4\uD2B8\uB97C \uC9C0\
  \uC6D0\uD569\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 어떻게:
Rust의 내장 테스트 프레임워크는 외부 라이브러리가 필요 없이 단위, 통합 및 문서 테스트를 지원합니다. 테스트는 `#[test]`로 주석 처리되며, 이와 같이 주석 처리된 모든 함수는 테스트로 컴파일됩니다.

### 단위 테스트 작성:
테스트하는 모듈 안에 `tests` 하위 모듈을 `#[cfg(test)]`로 마크하여 테스트할 때만 컴파일되도록 하여 단위 테스트를 배치합니다.

```rust
// lib.rs 또는 main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

테스트 실행:
```shell
$ cargo test
```

출력:
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (or src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### 통합 테스트 작성:
통합 테스트는 프로젝트 최상위에 있는 `src` 옆에 있는 tests 디렉토리에 위치합니다. `tests` 내의 각 `.rs` 파일은 자체 별개의 크레이트로 컴파일됩니다.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### 인기 있는 타사 라이브러리로 테스트하기:
더 광범위한 테스트 기능을 위해, `proptest` 라이브러리는 테스트 함수에 사용할 수 있는 다양한 입력을 생성할 수 있습니다.

`Cargo.toml`에 `proptest`를 개발 의존성으로 추가합니다:

```toml
[dev-dependencies]
proptest = "1.0"
```

`proptest`를 사용하여 많은 자동 생성 입력으로 동일한 테스트를 실행합니다:

```rust
// inside tests/integration_test.rs or a module's #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

이것은 `add`가 다양한 `i32` 입력에 대해 패닉하지 않는지 확인합니다.
