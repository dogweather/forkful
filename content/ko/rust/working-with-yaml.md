---
title:                "yaml 작업하기"
html_title:           "Rust: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
YAML 작업이란 무엇인가요? 이것은 데이터 직렬화 언어로서, 개발자들이 구조화된 데이터를 쉽게 표현하고 저장하며 전송하기 위해 사용됩니다. 이는 JSON과 같은 기존의 데이터 형식보다 더 간단하고 가독성이 높다는 장점이 있습니다.

개발자들이 YAML을 사용하는 이유는 무엇일까요? 그 이유는 YAML이 사람이 쉽게 읽고 쓸 수 있고, 파싱하기 간단한 구조를 가지고 있기 때문입니다. 또한, YAML은 다른 데이터 형식보다 더 유연하고 확장성이 뛰어나기 때문에, 프로젝트의 요구에 따라 쉽게 적용할 수 있습니다.

# 어떻게:
```Rust
use serde_yaml;
use std::fs::File;

// YAML 문서를 파일로 저장
let mut file = File::create("data.yaml")?;
let data = hashmap!{
    "name" => "John Doe",
    "age" => 27,
    "hobbies" => vec!["reading", "hiking", "coding"],
};
serde_yaml::to_writer(&mut file, &data)?;

// YAML 문서를 읽어오기
let file = File::open("config.yaml")?;
let config: HashMap<String, String> = serde_yaml::from_reader(file)?;

println!("Name: {}", config["name"]); // 출력: John Doe
```

```Rust
// 데이터를 YAML로 직렬화
let data = hashmap!{
    "name" => "Jane Smith",
    "age" => 30,
    "hobbies" => vec!["photography", "traveling", "yoga"],
};
let yaml_string = serde_yaml::to_string(&data)?;
println!("{}", yaml_string);

// 출력:
// name: Jane Smith
// age: 30
// hobbies:
//     - photography
//     - traveling
//     - yoga
```

# 깊이 들어가기:
YAML은 2001년에 발표되었으며, XML 및 JSON과 같은 다른 데이터 형식의 경량 대안으로 개발되었습니다. YAML은 전반적으로 간단하고 읽기 쉬운 문법을 가지고 있으며, 내장 도구를 통해 많은 다른 프로그래밍 언어와 호환되므로 많은 개발자들이 선호하는 형식 중 하나입니다.

YAML 외에도 대안으로 사용할 수 있는 데이터 직렬화 언어가 많이 있지만, 이 중에서도 JSON과 가장 비슷한 문법을 가진 TOML (Tom's Obvious, Minimal Language)이 널리 사용되고 있습니다. TOML은 YAML보다 조금 더 엄격한 문법을 가지고 있으며, 더 많은 구조화된 데이터를 표현할 수 있는 장점이 있습니다.

YAML을 Rust로 구현하기 위해서는 serde 라이브러리를 사용할 수 있으며, serde_yaml 모듈을 통해 바이트 순서로 직렬화하거나 텍스트로 인코딩하여 사용할 수 있습니다.

# 참고 자료:
- YAML 공식 웹사이트: https://yaml.org/
- Rust serde 라이브러리: https://docs.rs/serde/
- TOML 공식 웹사이트: https://toml.io/