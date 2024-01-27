---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
YAML은 데이터를 나타내기 위한 휴먼 리더블 형식입니다. 설정 파일, 데이터 교환 등을 위해 사용되며, 읽고 쓰기 쉬운 구조 때문에 개발자들이 많이 씁니다.

## How to: (어떻게 하기)
```Rust
use serde::{Serialize, Deserialize};
use serde_yaml;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    version: String,
    server: Server,
}

#[derive(Debug, Serialize, Deserialize)]
struct Server {
    ip: String,
    port: u16,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // YAML 데이터 읽기
    let data = r#"
version: "1.0"
server:
  ip: "192.168.1.1"
  port: 8080
"#;

    // 문자열에서 YAML 파싱
    let config: Config = serde_yaml::from_str(data)?;

    // 구조체 내용 출력
    println!("version: {}", config.version);
    println!("server.ip: {}", config.server.ip);
    println!("server.port: {}", config.server.port);

    Ok(())
}
```
출력:
```
version: 1.0
server.ip: 192.168.1.1
server.port: 8080
```

## Deep Dive (심층 분석)
YAML은 "YAML Ain't Markup Language"의 재귀적 약자로, 사람이 읽고 쓰기 쉬운 데이터 직렬화 표준입니다. JSON이나 XML과 비교할 때, YAML은 가독성에 더 초점을 맞추고 있습니다. Rust 에서는 `serde_yaml` 라이브러리를 주로 사용하여 YAML 데이터를 다룹니다. 이 라이브러리는 `serde`를 기반으로 하여 Rust 데이터 구조체와 YAML 데이터를 상호 변환할 수 있게 해 줍니다.

## See Also (참고 자료)
- Serde 공식 문서: https://serde.rs/
- `serde_yaml`크레이트: https://docs.rs/serde_yaml
- YAML 공식 웹사이트: https://yaml.org
- Rust 프로그래밍 언어: https://www.rust-lang.org/
