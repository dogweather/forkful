---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:49.883384-07:00
description: "\uBC29\uBC95: Rust\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  (`std`)\uB294 `std::path::Path` \uBC0F `std::fs` \uBAA8\uB4C8\uC744 \uD1B5\uD574\
  \ \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD558\
  \uB294 \uAE30\uB2A5\uC744 \uD3EC\uD568\uD558\uACE0 \uC788\uC2B5\uB2C8\uB2E4. \uB2E4\
  \uC74C\uC740 Rust\uC758 \uD45C\uC900 \uBC29\uC2DD\uC744 \uC0AC\uC6A9\uD558\uB294\
  \ \uAC04\uB2E8\uD55C \uC608\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.936784-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC(`std`)\uB294 `std::path::Path`\
  \ \uBC0F `std::fs` \uBAA8\uB4C8\uC744 \uD1B5\uD574 \uB514\uB809\uD1A0\uB9AC\uC758\
  \ \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uB294 \uAE30\uB2A5\uC744 \uD3EC\
  \uD568\uD558\uACE0 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

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
