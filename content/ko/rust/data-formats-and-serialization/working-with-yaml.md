---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:58.467053-07:00
description: "\uBC29\uBC95: Rust\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC5D0\uC11C YAML\uC744 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC73C\uBBC0\uB85C \uC77C\
  \uBC18\uC801\uC73C\uB85C `serde`(\uB370\uC774\uD130\uB97C \uC9C1\uB82C\uD654 \uBC0F\
  \ \uC5ED\uC9C1\uB82C\uD654\uD558\uB294 \uB370 \uC0AC\uC6A9)\uC640 `serde_yaml`\uC744\
  \ \uC870\uD569\uD558\uC5EC \uC11C\uB4DC \uD30C\uD2F0 \uD06C\uB808\uC774\uD2B8\uB97C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uBA3C\uC800, `Cargo.toml`\uC5D0 \uC758\uC874\uC131\
  \uC744 \uCD94\uAC00\uD558\uC138\uC694."
lastmod: '2024-03-13T22:44:54.945788-06:00'
model: gpt-4-0125-preview
summary: "Rust\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0\uC11C YAML\uC744\
  \ \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC73C\uBBC0\uB85C \uC77C\uBC18\uC801\uC73C\uB85C\
  \ `serde`(\uB370\uC774\uD130\uB97C \uC9C1\uB82C\uD654 \uBC0F \uC5ED\uC9C1\uB82C\uD654\
  \uD558\uB294 \uB370 \uC0AC\uC6A9)\uC640 `serde_yaml`\uC744 \uC870\uD569\uD558\uC5EC\
  \ \uC11C\uB4DC \uD30C\uD2F0 \uD06C\uB808\uC774\uD2B8\uB97C \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 방법:
Rust는 표준 라이브러리에서 YAML을 지원하지 않으므로 일반적으로 `serde`(데이터를 직렬화 및 역직렬화하는 데 사용)와 `serde_yaml`을 조합하여 서드 파티 크레이트를 사용합니다.

먼저, `Cargo.toml`에 의존성을 추가하세요.

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

이제, YAML 문자열을 Rust 구조체로 역직렬화하고 Rust 구조체를 다시 YAML 문자열로 직렬화하는 방법을 살펴보겠습니다.

### YAML을 Rust 구조체로 역직렬화하기
당신이 YAML에서 기대하는 데이터를 반영하는 Rust 구조체를 정의하세요. 필요한 경우 Serde 속성을 사용하여 사용자 정의를 할 수 있습니다.

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

위 Rust 코드를 실행하면 다음과 같은 샘플 출력이 나옵니다:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Rust 구조체를 YAML로 직렬화하기
이 예제는 이전 섹션의 `Config` 구조체를 다시 YAML 형식으로 직렬화합니다.

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

예상 출력은 YAML 형식 문자열일 것입니다:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

이 코드 조각들은 Rust 응용 프로그램에서 YAML 파싱 및 생성을 효율적으로 통합하는 방법을 보여줍니다. 이는 인기 있는 `serde` 및 `serde_yaml` 크레이트를 사용하여 복잡한 데이터 구조에 대응하고 간단하며 사람이 읽을 수 있는 구성을 제공합니다.
