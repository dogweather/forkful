---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:25.556036-07:00
description: "\uC5B4\uB5BB\uAC8C: Rust\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\uB824\uBA74\
  , \uC9C1\uB82C\uD654\uC640 \uC5ED\uC9C1\uB82C\uD654\uB97C \uC704\uD574 `serde` \uD06C\
  \uB808\uC774\uD2B8\uC640 `serde_json`\uC744 \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4. \uBA3C\uC800, `Cargo.toml`\uC5D0 \uC774\uB4E4\uC744 \uD3EC\
  \uD568\uC2DC\uD0A4\uB294 \uAC83\uC744 \uD655\uC778\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:54.947270-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\uB824\uBA74, \uC9C1\uB82C\uD654\
  \uC640 \uC5ED\uC9C1\uB82C\uD654\uB97C \uC704\uD574 `serde` \uD06C\uB808\uC774\uD2B8\
  \uC640 `serde_json`\uC744 \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 어떻게:
Rust에서 JSON을 다루려면, 직렬화와 역직렬화를 위해 `serde` 크레이트와 `serde_json`을 광범위하게 사용합니다. 먼저, `Cargo.toml`에 이들을 포함시키는 것을 확인하세요:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### 예제 1: JSON을 Rust 구조체로 역직렬화하기
Rust 구조체를 정의하고 `Deserialize` 및 `Serialize`를 위한 derive 매크로를 사용하세요:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "name": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let user: User = serde_json::from_str(json_data).unwrap();

    println!("사용자 ID: {}", user.id);
    println!("사용자 이름: {}", user.name);
    println!("사용자 이메일: {}", user.email);
}
```

**출력:**

```
사용자 ID: 1
사용자 이름: Jane Doe
사용자 이메일: jane.doe@example.com
```

### 예제 2: Rust 구조체를 JSON으로 직렬화하기
동일한 `User` 구조체를 사용하여:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**출력:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

이 예제들은 Rust 구조체를 JSON으로 역직렬화하고, Rust 구조체를 다시 JSON 문자열로 직렬화하는 기본적인 흐름을 보여줍니다. Serde는 선택적 필드, 복잡한 중첩, JSON에 의해 직접적으로 지원되지 않는 타입들을 다루는 것을 포함하여 JSON을 처리하기 위한 풍부한 도구 세트를 제공합니다.
