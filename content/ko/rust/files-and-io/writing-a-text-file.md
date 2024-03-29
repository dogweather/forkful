---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:18.314925-07:00
description: "Rust\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\uC131\uD558\
  \uB294 \uAC83\uC740 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC758 \uD30C\uC77C\uC744 \uC0DD\
  \uC131\uD558\uACE0, \uB370\uC774\uD130\uB97C \uC791\uC131\uD558\uBA70, \uD544\uC694\
  \uD55C \uACBD\uC6B0 \uB370\uC774\uD130\uB97C \uCD94\uAC00\uD558\uB294 \uC791\uC5C5\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uB85C\uADF8, \uAD6C\uC131, \uB610\uB294\
  \ \uC0AC\uC6A9\uC790 \uC0DD\uC131 \uCF58\uD150\uCE20\uC640 \uAC19\uC740 \uB370\uC774\
  \uD130\uB97C \uC601\uAD6C\uC801\uC73C\uB85C \uBCF4\uC874\uD558\uAE30 \uC704\uD574\
  \ \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4.\u2026"
lastmod: '2024-03-13T22:44:54.943006-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\uC131\uD558\
  \uB294 \uAC83\uC740 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC758 \uD30C\uC77C\uC744 \uC0DD\
  \uC131\uD558\uACE0, \uB370\uC774\uD130\uB97C \uC791\uC131\uD558\uBA70, \uD544\uC694\
  \uD55C \uACBD\uC6B0 \uB370\uC774\uD130\uB97C \uCD94\uAC00\uD558\uB294 \uC791\uC5C5\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uB85C\uADF8, \uAD6C\uC131, \uB610\uB294\
  \ \uC0AC\uC6A9\uC790 \uC0DD\uC131 \uCF58\uD150\uCE20\uC640 \uAC19\uC740 \uB370\uC774\
  \uD130\uB97C \uC601\uAD6C\uC801\uC73C\uB85C \uBCF4\uC874\uD558\uAE30 \uC704\uD574\
  \ \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4.\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Rust에서 텍스트 파일을 작성하는 것은 파일 시스템의 파일을 생성하고, 데이터를 작성하며, 필요한 경우 데이터를 추가하는 작업을 포함합니다. 프로그래머들은 애플리케이션 로그, 구성, 또는 사용자 생성 콘텐츠와 같은 데이터를 영구적으로 보존하기 위해 이 작업을 수행합니다. 이는 프로그램 실행 범위를 넘어서 데이터의 내구성을 보장합니다.

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
