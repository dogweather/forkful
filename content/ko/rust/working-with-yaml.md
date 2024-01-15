---
title:                "yaml과 함께 작업하기"
html_title:           "Rust: yaml과 함께 작업하기"
simple_title:         "yaml과 함께 작업하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜
이번 글에서는 YAML을 다루는 Rust 프로그래밍에 대해 알아보겠습니다. YAML은 사용하기 쉬운 구조로 데이터를 저장할 수 있게 해주는 형식이며, Rust는 안전하고 빠른 프로그래밍 언어로서 YAML을 다루는 데 적합한 도구입니다. 따라서 이 글을 통해 YAML을 더 편리하게 다룰 수 있는 방법에 대해 알아보시기 바랍니다!

## 어떻게
먼저, [YAML 라이브러리](https://crates.io/crates/yaml-rust)를 이용해 Rust에서 YAML을 다루는 방법을 알아보겠습니다. 코드 예시와 함께 설명하도록 하겠습니다. 

먼저, 아래의 코드를 `main.rs` 파일에 작성해주세요.
```Rust
use yaml_rust::{YamlLoader, YamlEmitter};

fn main() {
    let data = "
    invoice: 34843
    date   : 2001-01-23
    bill-to: &id001
        given  : Chris
        family : Dumars
        address:
            lines: |-
                458 Walkman Dr.
                Suite #292
            city    : Royal Oak
            state   : MI
            postal  : 48046";
    
    // YAML 파일 불러오기
    let docs = YamlLoader::load_from_str(data).unwrap();
    // YAML 데이터로부터 값을 가져오기
    let invoice = &docs[0]["invoice"];
    let date = &docs[0]["date"];
    let bill_to = &docs[0]["bill-to"];
    let address = &bill_to["address"];
    
    // 값 출력하기
    println!("Invoice: {}", invoice);
    println!("Date: {}", date);
    println!("Bill to:");
    println!("  Given: {}", &bill_to["given"]);
    println!("  Family: {}", &bill_to["family"]);
    println!("  Address:");
    println!("    Lines: {}", &address["lines"]);
    println!("    City: {}", &address["city"]);
    println!("    State: {}", &address["state"]);
    println!("    Postal: {}", &address["postal"]);
    
    // YAML 파일 생성하기
    let mut out_str = String::new();
    let mut emitter = YamlEmitter::new(&mut out_str);
    emitter.dump(docs[0]).unwrap();
    println!("{}", out_str);
}
```
위 코드에서는 `YamlLoader`와 `YamlEmitter`를 사용해 간단한 YAML 파일을 불러오고 값에 접근한 뒤, 새로운 YAML 파일을 생성하는 방법을 알 수 있습니다.

출력 예시는 다음과 같습니다.
```
Invoice: 34843
Date: 2001-01-23
Bill to:
  Given: Chris
  Family: Dumars
  Address:
    Lines: 458 Walkman Dr.
            Suite #292
    City: Royal Oak
    State: MI
    Postal: 48046
---
invoice: 34843
date: 2001-01-23
bill-to:
  given: Chris
  family: Dumars
  address:
    lines: |
      458 Walkman Dr.
          Suite #292
    city: Royal Oak
    state: MI
    postal: 48046
```

## 딥 다이브
[YAML 공식 사이트](https://yaml.org/)에서 YAML의 구문에 대해 더 자세히 알아볼 수 있습니다. 또한 [YAML 문서](https://yaml.org/spec/1.2/spec.html)에서 YAML 언어의 명세를 참고할 수 있습니다. 

Rust에서는 `yaml-rust` 라이브러리 외에도 `serde_yaml` 라이브러리를 사용해 YAML을 다룰 수 있습니다. `serde_yaml`은 YAML과 Rust 데이터 형식 간의 자동으로 직렬화 및 역직렬화를 수행해주는 기능을 제공합니다.

## See Also
- [YAML 공식 사이트](https://yaml