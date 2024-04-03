---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:28.921311-07:00
description: "CSV(Comma-Separated Values, \uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12\
  ) \uD30C\uC77C\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 \uD45C \uD615\uD0DC\uC758 \uB370\
  \uC774\uD130\uB97C \uC800\uC7A5\uD558\uB294 \uC77C\uBC18 \uD14D\uC2A4\uD2B8 \uD30C\
  \uC77C\uC5D0\uC11C \uC77D\uAC70\uB098 \uC4F0\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB2E4\uB978 \uD504\uB85C\uADF8\
  \uB7A8, \uC2DC\uC2A4\uD15C \uAC04\uC758 \uB370\uC774\uD130 \uACF5\uC720\uB97C \uAC00\
  \uB2A5\uD558\uAC8C \uD558\uAC70\uB098, \uB300\uC6A9\uB7C9 \uB370\uC774\uD130 \uC138\
  \uD2B8\uB97C \uD6A8\uC728\uC801\uC774\uACE0\u2026"
lastmod: '2024-03-13T22:44:54.948811-06:00'
model: gpt-4-0125-preview
summary: "CSV(Comma-Separated Values, \uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12\
  ) \uD30C\uC77C\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740 \uD45C \uD615\uD0DC\uC758 \uB370\
  \uC774\uD130\uB97C \uC800\uC7A5\uD558\uB294 \uC77C\uBC18 \uD14D\uC2A4\uD2B8 \uD30C\
  \uC77C\uC5D0\uC11C \uC77D\uAC70\uB098 \uC4F0\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\
  \uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

## 어떻게 하는가:
안전성과 성능에 중점을 둔 Rust는 `csv`가 가장 인기 있는 크레이트(라이브러리)를 비롯하여 CSV 파일을 다루기 위한 훌륭한 크레이트를 제공합니다. 또한, 데이터를 직렬화하고 역직렬화하는데 필요한 `serde`도 필요합니다.

먼저, 의존성을 `Cargo.toml`에 추가하세요:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### CSV 읽기
CSV 파일을 읽으려면 데이터를 나타내는 구조체를 정의하고 `serde`에서 `Deserialize`를 파생하세요:

```rust
use serde::Deserialize;
use std::error::Error;
use std::fs::File;
use std::io;
use std::process;

#[derive(Debug, Deserialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn read_from_csv(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file = File::open(file_path)?;
    let mut rdr = csv::Reader::from_reader(file);

    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = read_from_csv("cities.csv") {
        println!("example 실행중 에러 발생: {}", err);
        process::exit(1);
    }
}
```

도시 정보가 담긴 CSV의 샘플 출력은 다음과 같습니다:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### CSV 쓰기
CSV 파일에 쓰려면 구조체를 정의하고 `Serialize`를 파생하세요:

```rust
use serde::Serialize;
use std::error::Error;
use std::fs::File;

#[derive(Serialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn write_to_csv(file_path: &str, records: Vec<Record>) -> Result<(), Box<dyn Error>> {
    let file = File::create(file_path)?;
    let mut wtr = csv::Writer::from_writer(file);

    for record in records {
        wtr.serialize(&record)?;
    }
    wtr.flush()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let records = vec![
        Record {
            city: "Los Angeles".into(),
            state: "CA".into(),
            population: 3979563,
        },
        Record {
            city: "Chicago".into(),
            state: "IL".into(),
            population: 2695598,
        },
    ];

    write_to_csv("output.csv", records)?;

    Ok(())
}
```

이 작업은 데이터가 담긴 `output.csv`를 생성합니다:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

Rust의 강력한 타입 시스템과 견고한 생태계 크레이트를 활용함으로써, CSV 데이터를 다루는 작업은 효율적이면서도 간단해지며, 데이터 처리 작업에서 안전성과 성능을 보장합니다.
