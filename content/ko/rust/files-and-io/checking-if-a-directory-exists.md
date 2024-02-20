---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:49.883384-07:00
description: "\uC18C\uD504\uD2B8\uC6E8\uC5B4 \uAC1C\uBC1C\uC5D0\uC11C\uB294 \uD30C\
  \uC77C\uC5D0 \uC811\uADFC, \uC77D\uAE30, \uC4F0\uAE30\uB97C \uC2DC\uB3C4\uD560 \uB54C\
  \ \uC624\uB958\uB97C \uD53C\uD558\uAE30 \uC704\uD574 \uB514\uB809\uD1A0\uB9AC\uAC00\
  \ \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC774 \uC885\uC885\
  \ \uD544\uC694\uD569\uB2C8\uB2E4. \uC2DC\uC2A4\uD15C \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uC5B8\uC5B4\uC778 Rust\uB294 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\
  \ \uAC15\uB825\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\uC5EC \uD504\uB85C\uADF8\
  \uB7A8\uC774 \uD30C\uC77C\uACFC \uB514\uB809\uD1A0\uB9AC\uB97C \uC548\uC804\uD558\
  \uACE0 \uD6A8\uC728\uC801\uC73C\uB85C \uCC98\uB9AC\uD560 \uC218\u2026"
lastmod: 2024-02-19 22:05:13.842023
model: gpt-4-0125-preview
summary: "\uC18C\uD504\uD2B8\uC6E8\uC5B4 \uAC1C\uBC1C\uC5D0\uC11C\uB294 \uD30C\uC77C\
  \uC5D0 \uC811\uADFC, \uC77D\uAE30, \uC4F0\uAE30\uB97C \uC2DC\uB3C4\uD560 \uB54C\
  \ \uC624\uB958\uB97C \uD53C\uD558\uAE30 \uC704\uD574 \uB514\uB809\uD1A0\uB9AC\uAC00\
  \ \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC774 \uC885\uC885\
  \ \uD544\uC694\uD569\uB2C8\uB2E4. \uC2DC\uC2A4\uD15C \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uC5B8\uC5B4\uC778 Rust\uB294 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\
  \ \uAC15\uB825\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\uC5EC \uD504\uB85C\uADF8\
  \uB7A8\uC774 \uD30C\uC77C\uACFC \uB514\uB809\uD1A0\uB9AC\uB97C \uC548\uC804\uD558\
  \uACE0 \uD6A8\uC728\uC801\uC73C\uB85C \uCC98\uB9AC\uD560 \uC218\u2026"
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
소프트웨어 개발에서는 파일에 접근, 읽기, 쓰기를 시도할 때 오류를 피하기 위해 디렉토리가 존재하는지 확인하는 것이 종종 필요합니다. 시스템 프로그래밍 언어인 Rust는 이 작업을 수행하는 강력한 방법을 제공하여 프로그램이 파일과 디렉토리를 안전하고 효율적으로 처리할 수 있도록 합니다.

## 방법:
Rust의 표준 라이브러리(`std`)는 `std::path::Path` 및 `std::fs` 모듈을 통해 디렉토리의 존재 여부를 확인하는 기능을 포함하고 있습니다. 다음은 Rust의 표준 방식을 사용하는 간단한 예입니다:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");
    if path.exists() && path.is_dir() {
        println!("디렉토리가 존재합니다.");
    } else {
        println!("디렉토리가 존재하지 않습니다.");
    }
}
```

디렉토리가 존재한다고 가정한 샘플 출력:
```
디렉토리가 존재합니다.
```

보다 복잡한 시나리오나 향상된 기능(비동기 파일 시스템 작업과 같은)이 필요한 경우, 비동기 런타임 내에서 작업하는 경우 특히 `tokio`와 같은 타사 라이브러리 사용을 고려할 수 있습니다. 다음은 `tokio`를 사용하여 동일한 작업을 수행하는 방법입니다:

먼저, `Cargo.toml`에 `tokio`를 추가합니다:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

그런 다음, `tokio::fs`를 사용하여 비동기적으로 디렉토리가 존재하는지 확인합니다:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/path/to/directory";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("디렉토리가 존재합니다.");
            } else {
                println!("경로는 존재하지만 디렉토리가 아닙니다.");
            }
        },
        Err(_) => println!("디렉토리가 존재하지 않습니다."),
    }
}
```

디렉토리가 존재하지 않는다고 가정한 샘플 출력:
```
디렉토리가 존재하지 않습니다.
```

이 예들은 Rust와 그 생태계가 동기 및 비동기 접근법을 모두 제공하여 다양한 소프트웨어 개발 요구를 충족시킴을 강조합니다.
