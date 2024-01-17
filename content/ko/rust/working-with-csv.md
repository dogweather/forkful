---
title:                "CSV 파일 작업"
html_title:           "Rust: CSV 파일 작업"
simple_title:         "CSV 파일 작업"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV란 무엇인가요? 이것은 Comma-Separated Values의 약자로, 엑셀과 같은 표 형식의 데이터를 저장할 수 있는 형식입니다. 프로그래머들은 CSV를 사용하는 이유는 데이터를 쉽게 저장하고 공유할 수 있기 때문입니다.

## 사용 방법:

아래는 Rust를 사용하여 CSV 파일을 읽고 쓰는 예제 코드입니다.

```Rust
// CSV 파일 읽기
let mut rdr = csv::ReaderBuilder::new()
    .has_headers(false)
    .from_path("filename.csv")?;

for result in rdr.records() {
    let record = result?;
    println!("ID: {}, Name: {}, Age: {}", &record[0], &record[1], &record[2]);
}

// CSV 파일 쓰기
let mut wtr = csv::WriterBuilder::new()
    .from_path("output.csv")?;

wtr.write_record(&["1", "John", "25"])?;
wtr.write_record(&["2", "Mary", "30"])?;
wtr.flush()?;
```

## 심층 분석:

CSV는 1970년대부터 사용되어온 오래된 형식이며, 데이터를 읽고 쓰기에는 간단하지만 구조를 잘 이해해야 합니다. 그러나 이 형식은 다른 대체 형식들보다 범용성이 뛰어나며, 대부분의 데이터베이스 소프트웨어에서도 지원합니다. Rust에서는 `csv` 라이브러리를 사용하여 쉽게 CSV 파일을 처리할 수 있습니다.

## 관련 정보:

- [csv-rust 라이브러리 문서](https://docs.rs/csv/1.0.0/csv/)
- [CSV 파일 형식 정보](https://en.wikipedia.org/wiki/Comma-separated_values)