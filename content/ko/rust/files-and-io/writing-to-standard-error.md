---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:41.651805-07:00
description: "Rust\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(stderr)\uB85C \uC4F0\uAE30\
  \uB294 \uD45C\uC900 \uCD9C\uB825(stdout)\uACFC \uBCC4\uAC1C\uB85C \uC624\uB958 \uBA54\
  \uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uCF58\uC194\uC5D0 \uC804\uB2EC\uD558\uB294\
  \ \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC774\uB97C \uD1B5\uD574 \uC815\uC0C1 \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825\uACFC\
  \ \uC624\uB958 \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uBD84\uD558\uC5EC \uC624\uB958\uB97C\
  \ \uC801\uC808\uD788 \uCC98\uB9AC\uD558\uAC70\uB098 \uC2E4\uD589 \uC911\uC5D0 \uB85C\
  \uADF8\uB098 \uD30C\uC77C\uB85C \uB9AC\uB2E4\uC774\uB809\uD2B8\uD558\uAE30 \uC27D\
  \uAC8C\u2026"
lastmod: '2024-03-13T22:44:54.940036-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(stderr)\uB85C \uC4F0\uAE30\uB294\
  \ \uD45C\uC900 \uCD9C\uB825(stdout)\uACFC \uBCC4\uAC1C\uB85C \uC624\uB958 \uBA54\
  \uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uCF58\uC194\uC5D0 \uC804\uB2EC\uD558\uB294\
  \ \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
weight: 25
---

## 무엇이며 왜인가?
Rust에서 표준 오류(stderr)로 쓰기는 표준 출력(stdout)과 별개로 오류 메시지와 진단을 콘솔에 전달하는 것을 말합니다. 프로그래머들은 이를 통해 정상 프로그램 출력과 오류 메시지를 구분하여 오류를 적절히 처리하거나 실행 중에 로그나 파일로 리다이렉트하기 쉽게 만듭니다.

## 방법:
Rust는 `println!`이 stdout에 사용되는 것과 유사하게, `eprintln!` 매크로를 사용하여 stderr에 쓰는 간단한 방법을 제공합니다. 기본 예제는 다음과 같습니다:

```rust
fn main() {
    eprintln!("이것은 오류 메시지입니다!");
}
```

표준 오류에 대한 샘플 출력:
```
이것은 오류 메시지입니다!
```

텍스트를 포매팅하는 등 오류 메시지를 더 잘 제어하고 싶거나 I/O 결과를 처리하고 싶을 때는 `std::io` 모듈에서 `stderr` 함수를 사용합니다. 이 메서드는 전역 stderr 스트림에 대한 핸들을 제공하며, 여기에는 `Write` 트레잇에서 `write_all` 또는 `writeln` 같은 메서드를 사용하여 쓸 수 있습니다:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "포매팅된 오류 메시지: {}", 404).expect("stderr에 쓰기 실패");
}
```

표준 오류에 대한 샘플 출력:
```
포매팅된 오류 메시지: 404
```

`log`와 `env_logger` 같은 로깅이나 오류 처리를 위한 라이브러리에 의존하는 환경이나 애플리케이션에서 작업한다면, 이러한 라이브러리들이 인기가 있습니다. 이 라이브러리들은 주로 로깅 목적으로 사용되지만, 구성 가능하며 오류 로그 수준을 stderr로 직접할 수 있습니다. 다음은 `log`와 `env_logger`을 사용한 간단한 사용 예 입니다:

먼저, `Cargo.toml`에 의존성을 추가합니다:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

그 다음, 애플리케이션에서 로깅을 설정하고 사용합니다:
```rust
fn main() {
    env_logger::init();
    log::error!("이것은 stderr에 기록된 오류 메시지입니다");
}
```

이 프로그램을 실행하면(예를 들어 `RUST_LOG=error`와 같은 적절한 환경 변수로 `env_logger`를 설정한 후), 로깅 인프라를 사용하여 오류 메시지가 stderr에 출력됩니다.

```plaintext
ERROR: 이것은 stderr에 기록된 오류 메시지입니다
```
