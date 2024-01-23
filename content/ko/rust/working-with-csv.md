---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
CSV (Comma-Separated Values) 파일은 데이터를 저장하고 전송하는 가장 간단한 형식 중 하나입니다. 프로그래머들은 이를 사용하여 데이터를 쉽게 읽고, 쓰고, 공유하기 위해 사용합니다.

## How to: (어떻게 하나요?)
Rust에서 CSV 작업은 `csv` 크레이트를 사용해서 간단하게 할 수 있습니다. 먼저, `Cargo.toml`에 `csv` 크레이트를 추가해야 합니다.

```toml
[dependencies]
csv = "1.1.6"
```

다음은 표준 라이브러리에서 `std::io`를 사용한 예시 코드입니다.

```rust
use csv;
use std::error::Error;
use std::io;
use std::process;

fn main() {
    if let Err(err) = read_from_csv(io::stdin()) {
        println!("error running example: {}", err);
        process::exit(1);
    }
}

fn read_from_csv<R: io::Read>(rdr: R) -> Result<(), Box<dyn Error>> {
    let mut rdr = csv::Reader::from_reader(rdr);
    
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    
    Ok(())
}
```

실행 결과는 CSV 파일의 각 레코드를 라인별로 출력합니다.

## Deep Dive (깊이 있게 다루기)
CSV의 역사는 1970년대로 거슬러 올라가며, 텍스트를 처리하는 데 있어 가장 기본적인 수단 중 하나입니다. JSON이나 XML 같은 현대적인 포맷들이 등장했지만, CSV는 여전히 사람이 읽기 쉽고 소프트웨어 간 호환성이 높은 형식으로 널리 쓰입니다. Rust에서는 `csv` 크레이트를 통해 CSV 파일을 간단히 다룰 수 있으며, 성능도 뛰어납니다. 열 탐색, 열별 타입 주석, 헤더 기반의 직렬화 및 역직렬화 지원 등 고급 기능도 제공합니다.

## See Also (더 알아보기)
- Rust `csv` 크레이트 문서: [https://docs.rs/csv/latest/csv/](https://docs.rs/csv/latest/csv/)
- CSV 형식에 대한 자세한 설명: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
