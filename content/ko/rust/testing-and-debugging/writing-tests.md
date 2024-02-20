---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:09.066618-07:00
description: "Rust\uC5D0\uC11C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uCF54\uB4DC\uAC00 \uC608\uC0C1\uB300\uB85C \uC218\uD589\uB418\uB294\
  \uC9C0 \uD655\uC778\uD558\uAE30 \uC704\uD574 \uC790\uB3D9 \uAC80\uC0AC\uB97C \uC0DD\
  \uC131\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB294 \uC774\uB97C \uD1B5\uD574 \uCD08\uAE30\uC5D0 \uBC84\uADF8\uB97C\
  \ \uC7A1\uACE0, \uB9AC\uD329\uD1A0\uB9C1\uC744 \uC6A9\uC774\uD558\uAC8C \uD558\uBA70\
  , \uC2DC\uAC04\uC774 \uC9C0\uB0A8\uC5D0 \uB530\uB77C \uCF54\uB4DC \uD488\uC9C8\uC744\
  \ \uC720\uC9C0\uD569\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:13.824781
model: gpt-4-0125-preview
summary: "Rust\uC5D0\uC11C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uCF54\uB4DC\uAC00 \uC608\uC0C1\uB300\uB85C \uC218\uD589\uB418\uB294\
  \uC9C0 \uD655\uC778\uD558\uAE30 \uC704\uD574 \uC790\uB3D9 \uAC80\uC0AC\uB97C \uC0DD\
  \uC131\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB294 \uC774\uB97C \uD1B5\uD574 \uCD08\uAE30\uC5D0 \uBC84\uADF8\uB97C\
  \ \uC7A1\uACE0, \uB9AC\uD329\uD1A0\uB9C1\uC744 \uC6A9\uC774\uD558\uAC8C \uD558\uBA70\
  , \uC2DC\uAC04\uC774 \uC9C0\uB0A8\uC5D0 \uB530\uB77C \uCF54\uB4DC \uD488\uC9C8\uC744\
  \ \uC720\uC9C0\uD569\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Rust에서 테스트를 작성한다는 것은 코드가 예상대로 수행되는지 확인하기 위해 자동 검사를 생성하는 것을 의미합니다. 프로그래머는 이를 통해 초기에 버그를 잡고, 리팩토링을 용이하게 하며, 시간이 지남에 따라 코드 품질을 유지합니다.

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
