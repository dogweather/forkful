---
title:                "Rust: csv로 작업하기"
simple_title:         "csv로 작업하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜 CSV를 다루는 것인가?

CSV는 일반적으로 사용되는 데이터 형식 중 하나로, 다양한 어플리케이션에서 다루기 쉽고 보편적으로 이해할 수 있는 형식입니다. Rust는 안정성과 성능 강조를 통해 이러한 데이터를 처리하는 데 최적화되어 있습니다. 따라서 CSV를 다루는데 뛰어난 프로그래밍 언어로 Rust를 선택하는 것이 유용할 수 있습니다.

## 다루는 방법

우선, Rust 프로그래밍 언어를 먼저 설치해야 합니다. [공식 웹사이트](https://www.rust-lang.org/ko/tools/install)에서 다운로드 및 설치할 수 있습니다. 이후, CSV 데이터를 다루기 위해 다음 두 가지 라이브러리를 사용할 수 있습니다.

- [csv](https://github.com/BurntSushi/rust-csv): Rust로 CSV 파일을 읽고 쓰는 데 사용할 수 있는 라이브러리입니다. 이 라이브러리는 매우 간단하고 직관적인 API를 제공하여 사용하기 쉽습니다.
- [rust-csv-derive](https://github.com/BurntSushi/rust-csv): 이 라이브러리는 csv 라이브러리와 함께 사용하기 위한 유용한 매크로를 제공합니다. 매크로를 사용하면 생성한 구조체와 CSV 데이터를 매우 쉽게 매핑할 수 있습니다.

아래 코드 예시를 통해 csv 라이브러리를 사용하는 방법을 살펴보겠습니다.

```Rust
use csv::{Reader, Writer, Result, StringRecord};

fn read_csv() -> Result<()> {
    // CSV 파일을 읽기 위해 Reader를 생성합니다. 첫 번째 인자는 파일 이름, 두 번째 인자는 파싱 옵션입니다.
    let mut rdr = Reader::from_path("data.csv")?;
    // CSV 파일의 첫 번째 레코드를 읽고, 해당 레코드의 데이터 타입을 지정해 새로운 구조체를 생성합니다.
    let header_record = rdr.headers()?;
    let record: StringRecord = rdr.headers()?.deserialize(None)?;
    println!("First record: {}", record);
    // 더 이상 읽을 레코드가 없으면 에러 없이 정상적으로 종료합니다.
    Ok(())
}
```

출력 결과:

```
First record: Record { a: "1", b: "John", c: "Smith" }
```

마찬가지로 아래 코드 예시는 rust-csv-derive를 사용한 매크로 예시입니다.

```Rust
use csv::Writer;
use csv_destruct_macro::CsvRecord;

// CSV 파일의 데이터 타입을 매핑할 구조체를 생성합니다.
#[derive(CsvRecord)]
struct Person {
    #[csv(name = "column_a")]
    id: i32,
    #[csv(name = "column_b")]
    first_name: String,
    #[csv(name = "column_c")]
    last_name: String,
}

fn write_csv() -> Result<()> {
    // CSV 파일을 쓰기 위해 Writer를 생성합니다.
    let mut wtr = Writer::from_path("data.csv")?;
    // Person 구조체를 이용해 새로운 레코드를 생성하고 CSV 파일에 쓴 후에 Rust 타입으로 패킹합니다.
    let person = Person {
        id: 1,
        first_name: "John".to_owned(),
        last_name: "Smith".to_owned(),
    };
    wtr.serialize(person)?;
    // 더 이상 입력할 레코드가 없을 경우 에러 없이 정상적으로 종료합니다.
    Ok(())
}
```

출력 결과:

```csv
column_a, column_b, column_c
1, John, Smith
```

## 심화 학습

CSV 파일을 처리할