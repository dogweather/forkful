---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:41.651805-07:00
description: "\uBC29\uBC95: Rust\uB294 `println!`\uC774 stdout\uC5D0 \uC0AC\uC6A9\uB418\
  \uB294 \uAC83\uACFC \uC720\uC0AC\uD558\uAC8C, `eprintln!` \uB9E4\uD06C\uB85C\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC stderr\uC5D0 \uC4F0\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\
  \uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uAE30\uBCF8 \uC608\uC81C\uB294 \uB2E4\uC74C\
  \uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.940036-06:00'
model: gpt-4-0125-preview
summary: "Rust\uB294 `println!`\uC774 stdout\uC5D0 \uC0AC\uC6A9\uB418\uB294 \uAC83\
  \uACFC \uC720\uC0AC\uD558\uAC8C, `eprintln!` \uB9E4\uD06C\uB85C\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC stderr\uC5D0 \uC4F0\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\
  \uACF5\uD569\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
weight: 25
---

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
