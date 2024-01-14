---
title:                "Rust: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML은 간단한 포맷으로 데이터를 효율적으로 저장할 수 있는 것으로 유명합니다. 이러한 특징을 이용하여 더 효율적인 루스트 프로그래밍을 할 수 있습니다.

## 어떻게

먼저, YAML 외부 라이브러리를 이용해야 합니다. 우선Cargo.toml 파일에 다음의 의존성을 추가합니다:

```
[dependencies]
yaml-rust = "0.4.3"
```

그리고 코드를 작성하기 전에 무엇을 할 것인지 생각해야 합니다. 불러올 파일의 경로나 구성요소를 알아냅시다.

```
#[macro_use]
extern crate yaml_rust;

use std::fs::File;
use std::io::prelude::*;

use yaml_rust::YamlLoader;
```

이제 파일을 읽고 YAML로 파싱하는 코드를 작성합니다:

```
let mut file = File::open("config.yml").expect("파일을 열 수 없습니다.");

let mut contents = String::new();
file.read_to_string(&mut contents)
    .expect("파일을 읽을 수 없습니다.");

let yaml = YamlLoader::load_from_str(&contents).unwrap();
let docs = &yaml[0];
```

파일을 YAML로 읽었으니 이제 원하는 값을 불러와 사용할 수 있습니다. 예를 들어, 다음과 같이 작성합니다:

```
// "server" 맵 내부의 "port" 키 값을 가져오기
let port = docs["server"]["port"].as_i64().unwrap();

// "database" 맵 내부의 "username" 키 값을 가져오기
let username = docs["database"]["username"].as_str().unwrap();
```

## 깊이 파헤치기

YAML은 간단한 문법에 순서대로 값을 나열하는 것으로 이루어져 있습니다. 이렇게 구조화된 데이터를 다루는데 유용하며, 코드를 깔끔하고 읽기 쉽게 만들어줍니다. 또한 덕타이핑을 지원하므로 데이터 타입을 미리 정의할 필요가 없습니다. 루스트와 잘 어울리는 특징입니다.

## 또 다른 정보

[YAML 사용 예제 코드](https://docs.rs/yaml-rust/0.4.3/yaml_rust/)

[YAML 문법 공식 문서](https://yaml.org/spec/1.2/spec.html)

[루스트 언어 공식 사이트](https://www.rust-lang.org/)

[루스트와 YAML을 함께 사용하기](https://dev.to/deciduously/simple-rust-read-yaml-in-11-3ceg)