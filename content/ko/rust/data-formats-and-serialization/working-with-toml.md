---
date: 2024-01-26 04:26:30.893926-07:00
description: "\uBC29\uBC95: TOML\uC740 2013\uB144\uC5D0 Tom Preston-Werner\uC5D0 \uC758\
  \uD574 \uB9CC\uB4E4\uC5B4\uC9C4 \uAC83\uC73C\uB85C, Tom's Obvious, Minimal Language\uC758\
  \ \uC57D\uC790\uC785\uB2C8\uB2E4. \uC124\uC815 \uD30C\uC77C\uC5D0 JSON\uC774\uB098\
  \ YAML\uBCF4\uB2E4 \uB354 \uC77D\uAE30 \uC26C\uC6CC\uC57C \uD55C\uB2E4\uB294 \uBAA9\
  \uD45C\uB97C \uAC00\uC9C0\uACE0 \uC788\uC73C\uBA70, TOML\uC758 \uB514\uC790\uC778\
  \uC740 \uBAA8\uD638\uD558\uC9C0\u2026"
lastmod: '2024-04-05T21:53:56.734537-06:00'
model: gpt-4-0125-preview
summary: "TOML\uC740 2013\uB144\uC5D0 Tom Preston-Werner\uC5D0 \uC758\uD574 \uB9CC\
  \uB4E4\uC5B4\uC9C4 \uAC83\uC73C\uB85C, Tom's Obvious, Minimal Language\uC758 \uC57D\
  \uC790\uC785\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 방법:
```Rust
// 1. Cargo.toml에 'toml' 크레이트 포함
// [dependencies]
// toml = "0.5"

// 2. 러스트에서 구조체로 TOML 역직렬화
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("서버는 다음 주소에서 실행 중: {}:{}", host, port);
    // 출력: 서버는 다음 주소에서 실행 중: "localhost":8080
}
```

## 깊게 들여다보기
TOML은 2013년에 Tom Preston-Werner에 의해 만들어진 것으로, Tom's Obvious, Minimal Language의 약자입니다. 설정 파일에 JSON이나 YAML보다 더 읽기 쉬워야 한다는 목표를 가지고 있으며, TOML의 디자인은 모호하지 않은 문법, 최소주의, 그리고 데이터 유형에 쉽게 매핑되는 것에 초점을 맞추고 있습니다.

TOML의 대안으로는 JSON, YAML, 그리고 XML이 있지만, TOML은 인간의 가독성과 비프로그래머에 의한 파일 편집이 중요한 상황에서 승리합니다. 러스트에서 TOML을 작업할 때, serde는 직렬화와 역직렬화를 위한 탄탄한 기반을 제공하며, 특성을 사용하여 TOML을 러스트의 구조체에 손쉽게 매핑합니다.

TOML을 사용할 때의 한 가지 도전은 그것의 타입과 구조에 대한 엄격함입니다. 프로그래머는 TOML 데이터의 스키마를 반영하는 잘 구성된 러스트 타입 시스템을 정의해야만 러스트에서 TOML을 효과적으로 사용할 수 있습니다.

## 또한 보기
- [TOML 문서](https://toml.io/en/)
- [serde_toml 크레이트](https://docs.rs/serde_toml/)
- [러스트 프로그래밍 언어 책](https://doc.rust-lang.org/stable/book/)
- [TOML GitHub 저장소](https://github.com/toml-lang/toml)
