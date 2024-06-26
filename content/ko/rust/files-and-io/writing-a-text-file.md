---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:18.314925-07:00
description: "\uBC29\uBC95: Rust\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB294 \uD30C\uC77C \uC870\uC791\uC744 \uC704\uD55C \uAC15\uB825\uD55C \uB3C4\uAD6C\
  \uB97C \uC81C\uACF5\uD558\uBA70, \uC8FC\uB85C `std::fs` \uBC0F `std::io` \uBAA8\uB4C8\
  \ \uB0B4\uC5D0 \uCEA1\uC290\uD654\uB418\uC5B4 \uC788\uC2B5\uB2C8\uB2E4. \uD14D\uC2A4\
  \uD2B8 \uD30C\uC77C\uC744 \uC0DD\uC131\uD558\uACE0 \uC791\uC131\uD558\uAE30 \uC704\
  \uD55C \uAE30\uBCF8 \uC608\uC81C\uB294 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:54.943006-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 \uD30C\uC77C\
  \ \uC870\uC791\uC744 \uC704\uD55C \uAC15\uB825\uD55C \uB3C4\uAD6C\uB97C \uC81C\uACF5\
  \uD558\uBA70, \uC8FC\uB85C `std::fs` \uBC0F `std::io` \uBAA8\uB4C8 \uB0B4\uC5D0\
  \ \uCEA1\uC290\uD654\uB418\uC5B4 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:
Rust의 표준 라이브러리는 파일 조작을 위한 강력한 도구를 제공하며, 주로 `std::fs` 및 `std::io` 모듈 내에 캡슐화되어 있습니다. 텍스트 파일을 생성하고 작성하기 위한 기본 예제는 다음과 같습니다:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Hello, world!")?;
    Ok(())
}
```

이 코드를 실행한 후, "Hello, world!"라는 내용의 `hello.txt` 파일을 찾을 수 있습니다.

파일에 추가하거나 더 큰 데이터를 효율적으로 처리하는 등 더 복잡한 시나리오의 경우, Rust는 추가 기능을 제공합니다. 기존 파일에 텍스트를 추가하는 방법은 다음과 같습니다:

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" 추가된 텍스트.")?;
    Ok(())
}
```

이를 실행하면 `hello.txt`의 끝에 " 추가된 텍스트."가 추가됩니다.

일부 경우, 서드파티 라이브러리를 활용하면 파일 작업을 단순화할 수 있습니다. 예를 들어, `serde` 크레이트와 `serde_json`을 결합하면, 데이터 구조를 JSON 형식으로 직렬화 및 역직렬화할 수 있으며, 파일을 작성하는 고급 방법을 제공합니다:

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

위의 코드를 실행한 후, `user.json`은 `User` 구조체의 JSON 표현을 담게 됩니다. `serde`와 `serde_json`을 사용하려면 이 크레이트들을 `Cargo.toml`에 추가해야 한다는 점을 유의하세요.

Rust에서 텍스트 파일을 작성하는 것은 표준 라이브러리를 통하든 외부 크레이트의 도움을 받든, 애플리케이션에서 데이터 영속성을 관리하는 간단하면서도 강력한 방법입니다.
