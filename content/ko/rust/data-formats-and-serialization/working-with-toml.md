---
date: 2024-01-26 04:26:30.893926-07:00
description: "TOML\uC740 \uC885\uC885 \uC124\uC815 \uD30C\uC77C\uC5D0 \uC0AC\uC6A9\
  \uB418\uB294 \uC778\uAC04\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130\
  \ \uC9C1\uB82C\uD654 \uC5B8\uC5B4\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 TOML\uC758 \uB2E8\uC21C\uC131\uACFC \uBA85\uD655\uC131 \uB54C\uBB38\
  \uC5D0 \uADF8\uAC83\uC744 \uC0AC\uC6A9\uD558\uBA70, \uB7EC\uC2A4\uD2B8\uC5D0\uC11C\
  \ \uD574\uC2DC \uB9F5\uC73C\uB85C \uC27D\uAC8C \uBCC0\uD658\uB429\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.950205-06:00'
model: gpt-4-0125-preview
summary: "TOML\uC740 \uC885\uC885 \uC124\uC815 \uD30C\uC77C\uC5D0 \uC0AC\uC6A9\uB418\
  \uB294 \uC778\uAC04\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\
  \uB82C\uD654 \uC5B8\uC5B4\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 TOML\uC758 \uB2E8\uC21C\uC131\uACFC \uBA85\uD655\uC131 \uB54C\uBB38\uC5D0\
  \ \uADF8\uAC83\uC744 \uC0AC\uC6A9\uD558\uBA70, \uB7EC\uC2A4\uD2B8\uC5D0\uC11C \uD574\
  \uC2DC \uB9F5\uC73C\uB85C \uC27D\uAC8C \uBCC0\uD658\uB429\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML은 종종 설정 파일에 사용되는 인간이 읽을 수 있는 데이터 직렬화 언어입니다. 프로그래머들은 TOML의 단순성과 명확성 때문에 그것을 사용하며, 러스트에서 해시 맵으로 쉽게 변환됩니다.

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
