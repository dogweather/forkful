---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:28.921311-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB294\uAC00: \uC548\uC804\uC131\uACFC \uC131\
  \uB2A5\uC5D0 \uC911\uC810\uC744 \uB454 Rust\uB294 `csv`\uAC00 \uAC00\uC7A5 \uC778\
  \uAE30 \uC788\uB294 \uD06C\uB808\uC774\uD2B8(\uB77C\uC774\uBE0C\uB7EC\uB9AC)\uB97C\
  \ \uBE44\uB86F\uD558\uC5EC CSV \uD30C\uC77C\uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C\
  \ \uD6CC\uB96D\uD55C \uD06C\uB808\uC774\uD2B8\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4\
  . \uB610\uD55C, \uB370\uC774\uD130\uB97C \uC9C1\uB82C\uD654\uD558\uACE0 \uC5ED\uC9C1\
  \uB82C\uD654\uD558\uB294\uB370 \uD544\uC694\uD55C `serde`\uB3C4 \uD544\uC694\uD569\
  \uB2C8\uB2E4. \uBA3C\uC800, \uC758\uC874\uC131\uC744\u2026"
lastmod: '2024-03-13T22:44:54.948811-06:00'
model: gpt-4-0125-preview
summary: "\uC548\uC804\uC131\uACFC \uC131\uB2A5\uC5D0 \uC911\uC810\uC744 \uB454 Rust\uB294\
  \ `csv`\uAC00 \uAC00\uC7A5 \uC778\uAE30 \uC788\uB294 \uD06C\uB808\uC774\uD2B8(\uB77C\
  \uC774\uBE0C\uB7EC\uB9AC)\uB97C \uBE44\uB86F\uD558\uC5EC CSV \uD30C\uC77C\uC744\
  \ \uB2E4\uB8E8\uAE30 \uC704\uD55C \uD6CC\uB96D\uD55C \uD06C\uB808\uC774\uD2B8\uB97C\
  \ \uC81C\uACF5\uD569\uB2C8\uB2E4."
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
